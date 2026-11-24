;;;; win-ui-controls.lisp -- the basic controls on Windows, for the Swift calls of ios-swift.lisp: a switch (checkbox),
;;;; text field, multi-line text view, search bar, stepper, progress bar, spinner (marquee), page control, date
;;;; picker, month calendar and colour well.  Loads after win-host.lisp, in its package: each control is a Win32
;;;; child of the main window, its notifications come in through *MESSAGE-HOOKS* (BASIC-MESSAGE), and ui_set_value
;;;; reaches it through *SET-VALUE-HANDLERS* (keyed by the control's kind).
;;;;
;;;; Events: (:switch h 1|0), (:text h), (:step h value), (:date h seconds), (:page h index), (:color h rgb).
;;;; Limits: a page control and a stepper are several windows (dots, or a buddy and an up-down).  Their first window is
;;;; the handle's CTL-HWND; the others are in CTL-DATA, and ui_remove / ui_set_frame do not move them.

(in-package #:vessel-windows)

;;; ---- helpers ---------------------------------------------------------------------------------------------------------

(defmacro with-mem ((var size) &body body)
  "VAR: zeroed native memory of SIZE bytes, for BODY; freed after."
  (let ((s (gensym "SIZE")))
    `(let* ((,s ,size) (,var (modus-int::w32-alloc ,s t)))
       (unwind-protect (progn ,@body)
         (modus-int::w32-free ,var ,s)))))

(defun make-child (parent class text style x y w h)
  "A child window of PARENT (CHILD makes one of the main window): its class, text, style; points."
  (let ((hwnd (u *create-window* 0 (wide1 class) (wide2 text) (logior #x50000000 style)   ; WS_CHILD|WS_VISIBLE
                 (px x) (px y) (px w) (px h) parent 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no ~A" class))
    (c *send-message* hwnd #x30 *font* 1)                                                  ; WM_SETFONT
    hwnd))

(defun set-window-text-async (hwnd text)
  "HWND's text, set on the windows' thread without waiting (as SET-TEXT does)."
  (ui-post *set-window-text* (list hwnd (lambda (mem) (wide-at mem text)))
           (lambda (r mem) (declare (ignore r mem)) 0)
           (* 4 (+ 1 (length text)))))

(defun hex-text (rgb)
  "#RRGGBB for RGB (0xRRGGBB)."
  (let ((digits "0123456789ABCDEF"))
    (coerce (cons #\# (loop for shift from 20 downto 0 by 4
                            collect (char digits (ldb (byte 4 shift) rgb))))
            'string)))

(defun clamp-percent (v) (max 0 (min 100 v)))

;;; The epoch: seconds since 1970 (UTC) are universal time less this.
(defparameter *unix-universal-offset* 2208988800)

(defun seconds-to-systemtime (seconds mem)
  "SECONDS since 1970 (UTC) as a SYSTEMTIME at MEM (8 words)."
  (multiple-value-bind (sec minute hour date month year day)
      (decode-universal-time (+ seconds *unix-universal-offset*) 0)
    (zero mem 16)
    (poke16 mem year)
    (poke16 (+ mem 2) month)
    (poke16 (+ mem 4) (mod (1+ day) 7))                   ; Monday is 0 in Lisp, 1 in SYSTEMTIME; Sunday is 0 there
    (poke16 (+ mem 6) date)
    (poke16 (+ mem 8) hour)
    (poke16 (+ mem 10) minute)
    (poke16 (+ mem 12) sec)
    mem))

(defun systemtime-to-seconds (mem)
  (- (encode-universal-time (peek16 (+ mem 12)) (peek16 (+ mem 10)) (peek16 (+ mem 8)) (peek16 (+ mem 6))
                            (peek16 (+ mem 2)) (peek16 mem) 0)
     *unix-universal-offset*))

(defun set-systemtime (hwnd msg seconds)
  "DTM_SETSYSTEMTIME or MCM_SETCURSEL (MSG) with SECONDS."
  (with-mem (mem 16)
    (seconds-to-systemtime seconds mem)
    (c *send-message* hwnd msg 0 mem)))

(defun read-systemtime (hwnd msg)
  "DTM_GETSYSTEMTIME or MCM_GETCURSEL (MSG): the date the control holds, in seconds since 1970."
  (with-mem (mem 16)
    (c *send-message* hwnd msg 0 mem)
    (systemtime-to-seconds mem)))

(defun set-edit-text (hwnd text)
  (with-mem (mem (* 2 (+ 1 (length text))))
    (wide-at mem text)
    (c *send-message* hwnd #xC 0 mem)))                                                    ; WM_SETTEXT

(defun cue-banner (hwnd text)
  "EM_SETCUEBANNER: TEXT shown in the field while it is empty (the placeholder)."
  (with-mem (mem (* 2 (+ 1 (length text))))
    (wide-at mem text)
    (c *send-message* hwnd #x1501 1 mem)))

;;; ---- switch (BS_AUTOCHECKBOX) -----------------------------------------------------------------------------------------

(defun switch-new (on x y)
  (let* ((hwnd (make-child *hwnd* "BUTTON" "" #x3 x y 24 24))                              ; BS_AUTOCHECKBOX
         (v (if (and on (/= on 0)) 1 0)))
    (c *send-message* hwnd #xF1 v 0)                                                       ; BM_SETCHECK
    (new-handle (make-ctl :kind :switch :hwnd hwnd :value v))))

(defun switch-toggled (h)
  (let* ((ctl (ctl-of h))
         (v (if (= 1 (logand (c *send-message* (ctl-hwnd ctl) #xF0 0 0) #xFFFFFFFF)) 1 0)))  ; BM_GETCHECK
    (setf (ctl-value ctl) v)
    (ui-event (list :switch h v))))

(defun switch-set (h v)
  (let ((ctl (ctl-of h)) (on (if (and v (/= v 0)) 1 0)))
    (setf (ctl-value ctl) on)
    (c *send-message* (ctl-hwnd ctl) #xF1 on 0)))

;;; ---- text: field, view, search bar (EDIT) --------------------------------------------------------------------------

(defun text-field-new (a n x y w h)
  (let ((hwnd (make-child *hwnd* "EDIT" "" #x800080 x y w h)))                            ; WS_BORDER|ES_AUTOHSCROLL
    (cue-banner hwnd (utf8-text a n))
    (new-handle (make-ctl :kind :text-field :hwnd hwnd))))

;; A multi-line EDIT breaks lines only at CR LF: Lisp's newlines go in as CR LF and come back as newlines.
(defun crlf (text)
  (with-output-to-string (s)
    (loop for ch across text for prev = nil then cur for cur = ch
          do (when (and (char= ch #\Newline) (not (eql prev #\Return))) (write-char #\Return s))
             (write-char ch s))))
(defun lf (text) (remove #\Return text))

(defun text-view-new (a n x y w h)
  (let ((hwnd (make-child *hwnd* "EDIT" "" #xA01044 x y w h)))   ; WS_BORDER|WS_VSCROLL|ES_MULTILINE|ES_AUTOVSCROLL|ES_WANTRETURN
    (set-edit-text hwnd (crlf (utf8-text a n)))
    (new-handle (make-ctl :kind :text-view :hwnd hwnd))))

(setf (gethash :text-view *set-text-handlers*)
      (lambda (h text) (set-edit-text (ctl-hwnd (ctl-of h)) (crlf text)))
      (gethash :text-view *text-value-handlers*)
      (lambda (h) (lf (window-text (ctl-hwnd (ctl-of h))))))

(defun search-bar-new (a n x y w h)
  (let ((hwnd (make-child *hwnd* "EDIT" "" #x800080 x y w h)))
    (cue-banner hwnd (utf8-text a n))
    (new-handle (make-ctl :kind :search-bar :hwnd hwnd))))

;;; ---- stepper: an up-down (UPDOWN) with a buddy label showing its value --------------------------------------------
;;;
;;; Its window is the up-down; the buddy label (a STATIC: it sends no notifications, so no :text events) is in CTL-DATA.

(defun stepper-new (value x y)
  (let* ((v (clamp-percent value))
         (buddy (make-child *hwnd* "STATIC" (format nil "~D" v) #x1 x y 44 24))           ; SS_CENTER
         (up (make-child *hwnd* "msctls_updown32" "" #x22 (+ x 46) y 16 24)))             ; UDS_SETBUDDYINT|UDS_ARROWKEYS
    (c *send-message* up #x469 buddy 0)                                                    ; UDM_SETBUDDY
    (c *send-message* up #x46F 0 100)                                                      ; UDM_SETRANGE32 0..100
    (c *send-message* up #x471 0 v)                                                        ; UDM_SETPOS32
    (new-handle (make-ctl :kind :stepper :hwnd up :value v :data (list buddy)))))

(defun stepper-moved (h)
  "UDN_DELTAPOS: the up-down's position, read back."
  (let* ((ctl (ctl-of h))
         (v (s32 (c *send-message* (ctl-hwnd ctl) #x472 0 0))))                            ; UDM_GETPOS32
    (setf (ctl-value ctl) v)
    (set-window-text-async (first (ctl-data ctl)) (format nil "~D" v))
    (ui-event (list :step h v))))

(defun stepper-set (h v)
  (let* ((ctl (ctl-of h)) (v (clamp-percent v)))
    (setf (ctl-value ctl) v)
    (c *send-message* (ctl-hwnd ctl) #x471 0 v)                                            ; UDM_SETPOS32
    (set-window-text-async (first (ctl-data ctl)) (format nil "~D" v))))

;;; ---- progress bar and spinner (PROGRESS) ------------------------------------------------------------------------------

(defun progress-new (percent x y w h)
  (let* ((v (clamp-percent percent))
         (hwnd (make-child *hwnd* "msctls_progress32" "" 0 x y w h)))
    (c *send-message* hwnd #x402 v 0)                                                      ; PBM_SETPOS
    (new-handle (make-ctl :kind :progress :hwnd hwnd :value v))))

(defun spinner-new (x y w h)
  (let ((hwnd (make-child *hwnd* "msctls_progress32" "" #x8 x y w h)))                     ; PBS_MARQUEE
    (c *send-message* hwnd #x40A 1 30)                                                     ; PBM_SETMARQUEE: on, 30 ms a step
    (new-handle (make-ctl :kind :spinner :hwnd hwnd :value 1))))

(defun spinner-set (h v)
  (let* ((ctl (ctl-of h)) (on (if (and v (/= v 0)) 1 0)))
    (setf (ctl-value ctl) on)
    (c *send-message* (ctl-hwnd ctl) #x40A on 30)))                                         ; PBM_SETMARQUEE

;;; ---- page control: a row of dots, the current one lit ------------------------------------------------------------------
;;;
;;; Each dot is a small button under the main window (a button's notifications go to its parent, so they must reach
;;; the main window).  *DOTS* maps a dot's window to (handle . index).

(defvar *dots* (make-hash-table) "A page dot's window -> (handle . index).")

(defun dot-text (lit) (string (code-char (if lit #x25CF #x25CB))))                         ; ● and ○

(defun page-light (h i)
  (let* ((ctl (ctl-of h)) (dots (ctl-data ctl)))
    (setf (ctl-value ctl) i)
    (loop for d in dots for j from 0
          do (set-window-text-async d (dot-text (= j i))))))

(defun page-new (pages current x y)
  (let* ((n (max 1 pages))
         (dots (loop for i below n
                     collect (make-child *hwnd* "BUTTON" (dot-text (= i current)) 0 (+ x (* 20 i)) y 18 22)))
         (h (new-handle (make-ctl :kind :page :hwnd (first dots) :value current :data dots))))
    (loop for d in dots for i from 0 do (setf (gethash d *dots*) (cons h i)))
    h))

(defun page-tap (h i)
  (page-light h i)
  (ui-event (list :page h i)))

;;; ---- date picker and month calendar ---------------------------------------------------------------------------------

(defun date-new (seconds x y)
  (let ((hwnd (make-child *hwnd* "SysDateTimePick32" "" 0 x y 160 24)))
    (set-systemtime hwnd #x1002 seconds)                                                   ; DTM_SETSYSTEMTIME
    (new-handle (make-ctl :kind :date :hwnd hwnd :value seconds))))

(defun date-changed (h)
  "DTN_DATETIMECHANGE: the picker's date, read back."
  (let* ((ctl (ctl-of h))
         (secs (read-systemtime (ctl-hwnd ctl) #x1001)))                                   ; DTM_GETSYSTEMTIME
    (setf (ctl-value ctl) secs)
    (ui-event (list :date h secs))))

(defun calendar-new (seconds x y w h)
  (let ((hwnd (make-child *hwnd* "SysMonthCal32" "" 0 x y w h)))
    (set-systemtime hwnd #x1002 seconds)                                                   ; MCM_SETCURSEL
    (new-handle (make-ctl :kind :calendar :hwnd hwnd :value seconds))))

(defun calendar-picked (h)
  "MCN_SELECT: the day picked, read back with MCM_GETCURSEL."
  (let* ((ctl (ctl-of h))
         (secs (read-systemtime (ctl-hwnd ctl) #x1001)))                                   ; MCM_GETCURSEL
    (setf (ctl-value ctl) secs)
    (ui-event (list :date h secs))))

;;; ---- colour well: a button showing #RRGGBB, opening the colour dialog ------------------------------------------------

(defvar *choose-color* 0)

(defun choose-color-proc ()
  (when (zerop *choose-color*)
    (setf *choose-color* (modus-int::w32-proc "comdlg32.dll" "ChooseColorW")))
  (when (zerop *choose-color*) (error "vessel-windows: no ChooseColorW"))
  *choose-color*)

(defun color-well-new (rgb x y w h)
  (let* ((v (logand rgb #xFFFFFF))
         (hwnd (make-child *hwnd* "BUTTON" (hex-text v) 0 x y w h)))
    (new-handle (make-ctl :kind :color-well :hwnd hwnd :value v))))

(defun pick-colour (h)
  "The colour dialog, on the windows' thread: a colour chosen is a :color event (rgb as 0xRRGGBB)."
  (let* ((ctl (ctl-of h)) (rgb (or (ctl-value ctl) 0)) (hwnd (ctl-hwnd ctl)))
    (unlock-pointer)
    (ui-post (choose-color-proc)
             (list (lambda (mem)
                     ;; CHOOSECOLORW (72 bytes) and its 16 custom colours after it
                     (zero mem 160)
                     (poke32 mem 72)                                ; lStructSize
                     (poke64 (+ mem 8) *hwnd*)                      ; hwndOwner
                     (poke32 (+ mem 24) (colorref rgb))             ; rgbResult, COLORREF 0x00BBGGRR
                     (poke64 (+ mem 32) (+ mem 72))                 ; lpCustColors
                     (poke32 (+ mem 40) #x103)                      ; CC_RGBINIT|CC_FULLOPEN|CC_ANYCOLOR
                     mem))
             (lambda (r mem)
               (when (plusp (logand r #xFFFFFFFF))
                 (let ((new (colorref (peek32 (+ mem 24)))))
                   (setf (ctl-value ctl) new)
                   (set-window-text-async hwnd (hex-text new))
                   (ui-event (list :color h new)))))
             160)))

(defun color-well-set (h v)
  (let* ((ctl (ctl-of h)) (v (logand v #xFFFFFF)))
    (setf (ctl-value ctl) v)
    (set-window-text-async (ctl-hwnd ctl) (hex-text v))))

;;; ---- the notifications: WM_COMMAND (buttons, edits) and WM_NOTIFY (up-down, date picker, calendar) ---------------------

(defun basic-message (msg hwnd wp lp)
  "The message hook: true if it was one of these controls' notifications."
  (declare (ignore hwnd))
  (cond
    ((= msg #x111)                                                   ; WM_COMMAND: lp the control, (hi16 wp) the code
     (let ((dot (and (plusp lp) (gethash lp *dots*))))
       (if dot
           (progn (when (zerop (hi16 wp)) (page-tap (car dot) (cdr dot))) t)
           (let* ((h (and (plusp lp) (gethash lp *by-hwnd*)))
                  (ctl (and h (gethash h *ctls*)))
                  (code (hi16 wp)))
             (when ctl
               (case (ctl-kind ctl)
                 (:switch (when (zerop code) (switch-toggled h) t))                 ; BN_CLICKED
                 ((:text-field :text-view :search-bar)
                  (when (= code #x300) (ui-event (list :text h)) t))                ; EN_CHANGE
                 (:color-well (when (zerop code) (pick-colour h) t))))))))
    ((= msg #x4E)                                                    ; WM_NOTIFY: wp the control, lp the code
     (let* ((code (logand lp #xFFFFFFFF))
            (h (gethash wp *by-hwnd*))
            (ctl (and h (gethash h *ctls*))))
       (when ctl
         (case (ctl-kind ctl)
           (:stepper (when (= code #xFFFFFD2E) (stepper-moved h) t))         ; UDN_DELTAPOS  (-722)
           (:date (when (= code #xFFFFFD09) (date-changed h) t))             ; DTN_DATETIMECHANGE (-759)
           (:calendar (when (= code #xFFFFFD16) (calendar-picked h) t)))))) ; MCN_SELECT (-746)
    (t nil)))

(pushnew 'basic-message *message-hooks*)

;;; ---- the extension points ----------------------------------------------------------------------------------------------

(setf (gethash :switch *set-value-handlers*) #'switch-set
      (gethash :progress *set-value-handlers*) (lambda (h v)
                                                 (let ((ctl (ctl-of h)) (v (clamp-percent v)))
                                                   (setf (ctl-value ctl) v)
                                                   (c *send-message* (ctl-hwnd ctl) #x402 v 0)))   ; PBM_SETPOS
      (gethash :spinner *set-value-handlers*) #'spinner-set
      (gethash :stepper *set-value-handlers*) #'stepper-set
      (gethash :page *set-value-handlers*) (lambda (h v) (page-light h (max 0 (or v 0))))
      (gethash :date *set-value-handlers*) (lambda (h v)
                                             (let ((ctl (ctl-of h)))
                                               (setf (ctl-value ctl) v)
                                               (set-systemtime (ctl-hwnd ctl) #x1002 v)))
      (gethash :calendar *set-value-handlers*) (lambda (h v)
                                                 (let ((ctl (ctl-of h)))
                                                   (setf (ctl-value ctl) v)
                                                   (set-systemtime (ctl-hwnd ctl) #x1002 v)))
      (gethash :color-well *set-value-handlers*) #'color-well-set
      ;; a page's and a stepper's own window is a dot or the up-down: their text is their value, not a caption
      (gethash :page *set-text-handlers*) (lambda (h text) (declare (ignore h text)) nil)
      (gethash :stepper *set-text-handlers*) (lambda (h text) (declare (ignore h text)) nil)
      (gethash :page *text-value-handlers*) (lambda (h) (format nil "~D" (or (ctl-value (ctl-of h)) 0)))
      (gethash :stepper *text-value-handlers*) (lambda (h) (format nil "~D" (or (ctl-value (ctl-of h)) 0))))

;;; A stepper and a page control are more than one window: the buddy label beside the up-down, the dots after the
;;; first.  ui_remove takes them all; ui_set_frame moves them together, laid out as they were made.
(setf (gethash :stepper *remove-handlers*)
      (lambda (h) (dolist (w (ctl-data (ctl-of h))) (u *destroy-window* w)))
      (gethash :stepper *frame-handlers*)
      (lambda (h x y w hh)
        (declare (ignore w hh))
        (let ((ctl (ctl-of h)))
          (c *set-window-pos* (first (ctl-data ctl)) 0 (px x) (px y) (px 44) (px 24) #x14)
          (c *set-window-pos* (ctl-hwnd ctl) 0 (px (+ x 46)) (px y) (px 16) (px 24) #x14)))
      (gethash :page *remove-handlers*)
      (lambda (h)
        (dolist (d (rest (ctl-data (ctl-of h)))) (remhash d *dots*) (u *destroy-window* d))
        (remhash (ctl-hwnd (ctl-of h)) *dots*))
      (gethash :page *frame-handlers*)
      (lambda (h x y w hh)
        (declare (ignore w hh))
        (loop for d in (ctl-data (ctl-of h)) for i from 0
              do (c *set-window-pos* d 0 (px (+ x (* 20 i))) (px y) (px 18) (px 22) #x14))))

;;; ---- the calls -----------------------------------------------------------------------------------------------------------

(defswift "ui_switch_new" (on x y) (switch-new on x y))
(defswift "ui_text_field_new" (a n x y w h) (text-field-new a n x y w h))
(defswift "ui_text_view_new" (a n x y w h) (text-view-new a n x y w h))
(defswift "ui_search_bar_new" (a n x y w h) (search-bar-new a n x y w h))
(defswift "ui_stepper_new" (value x y) (stepper-new value x y))
(defswift "ui_progress_new" (percent x y w h) (progress-new percent x y w h))
(defswift "ui_spinner_new" (x y w h) (spinner-new x y w h))
(defswift "ui_page_new" (pages current x y) (page-new pages current x y))
(defswift "ui_date_new" (seconds x y) (date-new seconds x y))
(defswift "ui_calendar_new" (seconds x y w h) (calendar-new seconds x y w h))
(defswift "ui_color_well_new" (rgb x y w h) (color-well-new rgb x y w h))
