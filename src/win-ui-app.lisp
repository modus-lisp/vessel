;;;; win-ui-app.lisp -- the native app layer (ios-native.lisp's kiln-native, Native.swift's app_* calls) on Windows: a
;;;; navigation stack with its bar, screens, tables, a compose bar, forms.  What an ordinary phone app is built of
;;;; (chirp's chat), on top of win-host.lisp and the widget files (the list is win-ui-choices' ListView, the menus its
;;;; popup).  Loaded by (vessel-windows:load-widgets).
;;;;
;;;; THE LAYOUT.  A navigation (app_nav_present) fills the window: a bar along the top -- the back button when a
;;;; screen is pushed, the screen's bar buttons left and right, its title between -- and the screens under it, one
;;;; shown at a time.  Every view is a child of the main window, placed absolutely; a screen is the views it made, and
;;;; pushing hides the one under it.  A table fills its screen (less the compose bar, if it has one).
;;;;
;;;; Events, as Native.swift sends them: a bar button's :tap; a bar menu's (:menu button id); a table row's
;;;; (:select table index); the compose bar's (:compose compose text); a form's (:form form (1 value ...)) on Save,
;;;; (:form form (0)) on Cancel; back is (:pop screen).
;;;;
;;;; Approximated: no slide animations; a table row shows "title -- subtitle" on one line; the compose bar has no
;;;; placeholder (a multi-line edit, so Enter sends, takes none); SF Symbols in bar buttons are a few glyphs or the
;;;; name.

(in-package #:vessel-windows)

(defvar *ap-get-window-rect* (modus-int::w32-proc "user32.dll" "GetWindowRect"))
(defvar *ap-get-time-zone* (modus-int::w32-proc "kernel32.dll" "GetTimeZoneInformation"))

(defparameter *ap-background* #x16181c)
(defparameter *ap-bar-colour* #x1f2a38)
(defparameter *ap-bar-h* 48)
(defparameter *ap-compose-h* 48)

(defstruct ap-nav bg bar title back (screens '()) root handle)
(defstruct ap-screen nav handle (title "") (views '()) (left '()) (right '()) table compose empty (empty-text nil))

(defun ap-points (pixels) (round (* pixels 96) *dpi*))
(defun ap-size () (multiple-value-bind (w h) (hwnd-size *hwnd*) (values (ap-points w) (ap-points h))))

(defun ap-view (class text style x y w h)
  "A child of the main window at X Y W H points, above what is there."
  (let ((hw (child class text style x y w h)))
    (raise hw)
    hw))

(defun ap-show (hwnds on) (dolist (hw hwnds) (u *show-window* hw (if on 5 0))))

(defun ap-drop (hwnd)
  (let ((h (gethash hwnd *by-hwnd*)))
    (when h (remhash h *ctls*) (remhash h *on-remove*))
    (remhash hwnd *by-hwnd*)
    (u *destroy-window* hwnd)))

(defun ap-glyph (name)
  "An SF Symbol's name as a bar button's text: a glyph for the common ones, else the name."
  (cond ((string= name "xmark") (string (code-char #x2715)))
        ((string= name "ellipsis") (string (code-char #x22EF)))
        ((string= name "plus") "+")
        ((string= name "chevron.left") (string (code-char #x2039)))
        ((search "gear" name) (string (code-char #x2699)))
        (t name)))

;;; ---- the navigation ------------------------------------------------------------------------------------------------

(defun ap-top (nav) (first (ap-nav-screens nav)))

(defun ap-relayout-bar (nav)
  "The top screen's title, and its bar buttons, on the bar; back when there is a screen under it."
  (let ((s (ap-top nav)))
    (when s
      (ui-post *set-window-text* (list (ap-nav-title nav) (lambda (mem) (wide-at mem (ap-screen-title s))))
               (lambda (r mem) r mem) (* 4 (+ 1 (length (ap-screen-title s)))))
      (u *show-window* (ap-nav-back nav) (if (rest (ap-nav-screens nav)) 5 0)))))

(defun ap-screen-new (nav title)
  "A screen on NAV, shown, the one under it hidden: its handle."
  (let ((under (ap-top nav))
        (s (make-ap-screen :nav nav :title title)))
    (when under
      (ap-show (append (ap-screen-views under) (ap-screen-left under) (ap-screen-right under)) nil))
    (push s (ap-nav-screens nav))
    (setf (ap-screen-handle s) (new-handle (make-ctl :kind :app-screen :data s)))
    (ap-relayout-bar nav)
    (ap-screen-handle s)))

(defun ap-pop (nav)
  "The top screen goes, and the one under it shows again: (:pop screen)."
  (let ((s (pop (ap-nav-screens nav))))
    (when s
      (dolist (hw (append (ap-screen-views s) (ap-screen-left s) (ap-screen-right s))) (ap-drop hw))
      (remhash (ap-screen-handle s) *ctls*)
      (let ((under (ap-top nav)))
        (when under (ap-show (append (ap-screen-views under) (ap-screen-left under) (ap-screen-right under)) t)
              (ap-table-refresh-empty under)))
      (ap-relayout-bar nav)
      (ui-event (list :pop (ap-screen-handle s))))))

(defun ap-nav-present (title)
  (multiple-value-bind (w h) (ap-size)
    (let* ((bg (funcall (gethash "ui_panel_new" *swift*) 0 0 w h *ap-background*))
           (bar (funcall (gethash "ui_panel_new" *swift*) 0 0 w *ap-bar-h* *ap-bar-colour*))
           (title-hw (ap-view "STATIC" "" #x201 80 0 (- w 160) *ap-bar-h*))           ; SS_CENTER|SS_CENTERIMAGE
           (back (ap-view "BUTTON" (format nil "~A Back" (code-char #x2039)) 0 8 8 72 32))
           (nav (make-ap-nav :bg bg :bar bar :title title-hw :back back)))
      (u *show-window* back 0)
      ;; no window of its own: registering one raises it, and the background would cover the bar
      (setf (ap-nav-handle nav) (new-handle (make-ctl :kind :app-nav :data nav)))
      (setf (gethash back *ap-backs*) nav)
      (setf (ap-nav-root nav) (ap-screen-new nav title))
      (ap-nav-handle nav))))

(defvar *ap-backs* (make-hash-table) "A navigation's back button -> the navigation.")

(defun ap-nav-of (h) (let ((ctl (gethash h *ctls*))) (and ctl (eq (ctl-kind ctl) :app-nav) (ctl-data ctl))))
(defun ap-screen-of (h) (let ((ctl (gethash h *ctls*))) (and ctl (eq (ctl-kind ctl) :app-screen) (ctl-data ctl))))

(defun ap-nav-dismiss (h)
  (let ((nav (ap-nav-of h)))
    (when nav
      (loop while (ap-nav-screens nav)
            do (let ((s (pop (ap-nav-screens nav))))
                 (dolist (hw (append (ap-screen-views s) (ap-screen-left s) (ap-screen-right s))) (ap-drop hw))
                 (remhash (ap-screen-handle s) *ctls*)))
      (remhash (ap-nav-back nav) *ap-backs*)
      (dolist (hw (list (ap-nav-back nav) (ap-nav-title nav))) (ap-drop hw))
      (dolist (p (list (ap-nav-bar nav) (ap-nav-bg nav))) (ap-drop (ctl-hwnd (ctl-of p))) (remhash p *ctls*))
      (remhash h *ctls*)))
  0)

(defun ap-set-title (h title)
  (let ((s (ap-screen-of h)))
    (when s
      (setf (ap-screen-title s) title)
      (when (eq s (ap-top (ap-screen-nav s))) (ap-relayout-bar (ap-screen-nav s)))))
  0)

;;; ---- bar buttons and bar menus ----------------------------------------------------------------------------------------

(defvar *ap-menus* (make-hash-table) "A bar menu's button hwnd -> (handle items).")

(defun ap-bar-slot (s right)
  "Where SCREEN's next bar button goes: (x y), from the left after back, or from the right edge."
  (multiple-value-bind (w h) (ap-size)
    (declare (ignore h))
    (let ((k (length (if right (ap-screen-right s) (ap-screen-left s)))))
      (if right
          (list (- w 8 (* 48 (1+ k))) 8)
          (list (+ (if (rest (ap-nav-screens (ap-screen-nav s))) 88 8) (* 48 k)) 8)))))

(defun ap-bar-button (h text right)
  (let ((s (ap-screen-of h)))
    (if (null s)
        0
        (destructuring-bind (x y) (ap-bar-slot s right)
          (let ((hw (ap-view "BUTTON" (ap-glyph text) 0 x y 40 32)))
            (if right (push hw (ap-screen-right s)) (push hw (ap-screen-left s)))
            (new-handle (make-ctl :kind :button :hwnd hw)))))))

(defun ap-bar-menu (h spec)
  (let* ((s (ap-screen-of h))
         (b (and s (ap-bar-button h (or (getf spec :symbol) "ellipsis") (getf spec :right)))))
    (when (and b (plusp b))
      (setf (gethash (ctl-hwnd (ctl-of b)) *ap-menus*) (list b (getf spec :items))))
    (or b 0)))

(defun ap-menu-click (hw)
  "A bar menu's button: its items as a popup under it; the one chosen is (:menu button id)."
  (destructuring-bind (b items) (gethash hw *ap-menus*)
    (let ((r (area 6)))
      (c *ap-get-window-rect* hw r)
      (choice-popup (mapcar (lambda (it) (or (getf it :title) "")) items)
                    (s32 (peek32 r)) (s32 (peek32 (+ r 12)))
                    (lambda (index)
                      (when index (ui-event (list :menu b (getf (nth index items) :id)))))))))

;;; ---- tables --------------------------------------------------------------------------------------------------------

(defun ap-content (s)
  "SCREEN's content area, (x y w h) in points: under the bar, above its compose bar."
  (declare (ignore s))
  (multiple-value-bind (w h) (ap-size)
    (list 0 *ap-bar-h* w (- h *ap-bar-h*))))

(defun ap-table (h plain)
  (declare (ignore plain))
  (let ((s (ap-screen-of h)))
    (if (null s)
        0
        (destructuring-bind (x y w hh) (ap-content s)
          (let* ((table (list-new x y w (- hh (if (ap-screen-compose s) *ap-compose-h* 0))))
                 (hw (ctl-hwnd (ctl-of table))))
            (raise hw)
            (push hw (ap-screen-views s))
            (setf (ap-screen-table s) table
                  (gethash table *ap-table-screens*) s)
            table)))))

(defvar *ap-table-screens* (make-hash-table) "A table's handle -> its screen.")
(defvar *ap-rows* (make-hash-table) "A table's handle -> how many rows it has.")

(defun ap-table-set (table rows)
  (list-set-data table (token (vector rows)) 1)
  (setf (gethash table *ap-rows*) (length rows))
  (let ((s (gethash table *ap-table-screens*))) (when s (ap-table-refresh-empty s)))
  0)

(defun ap-table-refresh-empty (s)
  "The empty-state text over SCREEN's table while it has no rows."
  (let ((table (ap-screen-table s)) (empty (ap-screen-empty s)))
    (when (and table empty)
      (u *show-window* empty (if (zerop (gethash table *ap-rows* 0)) 5 0)))))

(defun ap-table-empty (table spec)
  (let ((s (gethash table *ap-table-screens*)))
    (when s
      (let ((text (format nil "~A~%~%~A" (or (getf spec :title) "") (or (getf spec :message) ""))))
        (if (ap-screen-empty s)
            (ui-post *set-window-text* (list (ap-screen-empty s) (lambda (mem) (wide-at mem text)))
                     (lambda (r mem) r mem) (* 4 (+ 1 (length text))))
            (destructuring-bind (x y w hh) (ap-content s)
              (declare (ignore hh))
              (let ((hw (ap-view "STATIC" text #x1 (+ x 24) (+ y 120) (- w 48) 90)))     ; SS_CENTER
                (push hw (ap-screen-views s))
                (setf (ap-screen-empty s) hw))))
        (ap-table-refresh-empty s))))
  0)

(defun ap-table-scroll-end (table)
  (let* ((hw (ctl-hwnd (ctl-of table)))
         (n (logand (c *send-message* hw #x1004 0 0) #xFFFFFFFF)))                        ; LVM_GETITEMCOUNT
    (when (plusp n) (c *send-message* hw #x1013 (1- n) 0)))                              ; LVM_ENSUREVISIBLE
  0)

;;; ---- the compose bar -------------------------------------------------------------------------------------------------

(defvar *ap-composes* (make-hash-table) "A compose bar's edit or Send button hwnd -> its handle.")

(defun ap-compose (h)
  (let ((s (ap-screen-of h)))
    (if (null s)
        0
        (multiple-value-bind (w hh) (ap-size)
          (let* ((y (- hh *ap-compose-h*))
                 (edit (ap-view "EDIT" "" #x801044 8 (+ y 8) (- w 96) 32))        ; WS_BORDER|ES_MULTILINE|ES_WANTRETURN|ES_AUTOVSCROLL
                 (send (ap-view "BUTTON" "Send" 0 (- w 80) (+ y 8) 72 32))
                 (handle (new-handle (make-ctl :kind :app-compose :hwnd edit :data send))))
            (setf (gethash edit *ap-composes*) handle (gethash send *ap-composes*) handle)
            (push edit (ap-screen-views s)) (push send (ap-screen-views s))
            (setf (ap-screen-compose s) handle)
            ;; the table, if made first, gives up the bottom
            (when (ap-screen-table s)
              (destructuring-bind (x yy cw ch) (ap-content s)
                (c *set-window-pos* (ctl-hwnd (ctl-of (ap-screen-table s))) 0 (px x) (px yy) (px cw)
                   (px (- ch *ap-compose-h*)) #x14)))
            handle)))))

(defun ap-compose-send (handle)
  "What the compose bar holds, sent (:compose handle text) and cleared -- unless it is only blank."
  (let* ((edit (ctl-hwnd (ctl-of handle)))
         (text (string-trim '(#\Space #\Tab #\Return #\Newline) (window-text edit))))
    (when (plusp (length text))
      (ui-event (list :compose handle (remove #\Return (substitute #\Space #\Newline text)))))
    (ui-post *set-window-text* (list edit (lambda (mem) (wide-at mem ""))) (lambda (r mem) r mem) 8)))

;;; ---- forms -----------------------------------------------------------------------------------------------------------

(defstruct ap-form title save (fields '()) (notes '()) handle (views '()) (controls '()))

(defun ap-form-new (title save)
  (let ((f (make-ap-form :title title :save save)))
    (setf (ap-form-handle f) (new-handle (make-ctl :kind :app-form :data f)))))

(defun ap-form-of (h) (let ((ctl (gethash h *ctls*))) (and ctl (eq (ctl-kind ctl) :app-form) (ctl-data ctl))))

(defun ap-form-field (h spec)
  (let ((f (ap-form-of h)))
    (if f (progn (setf (ap-form-fields f) (append (ap-form-fields f) (list spec))) (1- (length (ap-form-fields f)))) 0)))

(defun ap-form-note (h text)
  (let ((f (ap-form-of h))) (when f (setf (ap-form-notes f) (append (ap-form-notes f) (list text))))) 0)

(defvar *ap-form-buttons* (make-hash-table) "A form's Save or Cancel hwnd -> (form . saved-p).")

(defun ap-form-show (h)
  "The form over the window: a bar (Cancel, its title, Save), and a row per field, its notes under them."
  (let ((f (ap-form-of h)))
    (when f
      (multiple-value-bind (w hh) (ap-size)
        (let* ((bg (funcall (gethash "ui_panel_new" *swift*) 0 0 w hh *ap-background*))
               (bar (funcall (gethash "ui_panel_new" *swift*) 0 0 w *ap-bar-h* *ap-bar-colour*))
               (cancel (ap-view "BUTTON" "Cancel" 0 8 8 72 32))
               (title (ap-view "STATIC" (ap-form-title f) #x201 88 0 (- w 176) *ap-bar-h*))
               (save (ap-view "BUTTON" (ap-form-save f) 0 (- w 80) 8 72 32))
               (y (+ *ap-bar-h* 16)))
          (setf (ap-form-views f) (list (ctl-hwnd (ctl-of bg)) (ctl-hwnd (ctl-of bar)) cancel title save))
          (setf (gethash cancel *ap-form-buttons*) (cons f nil) (gethash save *ap-form-buttons*) (cons f t))
          (dolist (spec (ap-form-fields f))
            (let ((label (ap-view "STATIC" (or (getf spec :label) "") 0 16 y 110 28))
                  (control
                    (case (getf spec :kind 0)
                      (4 (let ((b (ap-view "BUTTON" "" #x3 132 y 28 28)))                       ; BS_AUTOCHECKBOX
                           (when (equal (getf spec :value) "1") (c *send-message* b #xF1 1 0))   ; BM_SETCHECK
                           b))
                      (5 (let ((cb (ap-view "COMBOBOX" "" #x203 132 y (- w 148) 200)))          ; CBS_DROPDOWNLIST
                           (dolist (ch (getf spec :choices)) (c *send-message* cb #x143 0 (wide1 ch)))   ; CB_ADDSTRING
                           (c *send-message* cb #x14E (or (ignore-errors (parse-integer (getf spec :value))) 0) 0)
                           cb))
                      (t (let ((e (ap-view "EDIT" (or (getf spec :value) "")
                                           (logior #x800080 (if (eql (getf spec :kind) 2) #x20 0))   ; ES_PASSWORD
                                           132 y (- w 148) 28)))
                           (c *send-message* e #x1501 1 (wide1 (or (getf spec :placeholder) "")))     ; EM_SETCUEBANNER
                           e)))))
              (push label (ap-form-views f))
              (push control (ap-form-views f))
              (setf (ap-form-controls f) (append (ap-form-controls f) (list (cons (getf spec :kind 0) control))))
              (incf y 40)))
          (dolist (note (ap-form-notes f))
            (push (ap-view "STATIC" note 0 16 (+ y 8) (- w 32) 60) (ap-form-views f))
            (incf y 68))))))
  0)

(defun ap-form-close (f saved)
  "(:form form (1 value ...)) for Save, (:form form (0)) for Cancel; the form's views go."
  (let ((values (when saved
                  (mapcar (lambda (kc)
                            (destructuring-bind (kind . hw) kc
                              (case kind
                                (4 (if (= 1 (logand (c *send-message* hw #xF0 0 0) #xFFFFFFFF)) "1" "0"))   ; BM_GETCHECK
                                (5 (princ-to-string (logand (c *send-message* hw #x147 0 0) #xFFFFFFFF)))   ; CB_GETCURSEL
                                (t (window-text hw)))))
                          (ap-form-controls f)))))
    (dolist (hw (ap-form-views f))
      (remhash hw *ap-form-buttons*)
      (let ((h (gethash hw *by-hwnd*))) (when h (remhash h *ctls*)))
      (remhash hw *by-hwnd*)
      (u *destroy-window* hw))
    (setf (ap-form-views f) '() (ap-form-controls f) '())
    (ui-event (list :form (ap-form-handle f) (if saved (cons 1 values) (list 0))))))

;;; ---- clicks and typing -------------------------------------------------------------------------------------------

(defun ap-message-hook (msg hwnd wp lp)
  "Back, a bar menu, a compose bar's Send (or Enter in it), a form's Save and Cancel."
  (declare (ignore hwnd))
  (when (and (= msg #x111) (plusp lp))
    (let ((code (hi16 wp)))
      (cond ((and (zerop code) (gethash lp *ap-backs*)) (ap-pop (gethash lp *ap-backs*)) t)
            ((and (zerop code) (gethash lp *ap-menus*)) (ap-menu-click lp) t)
            ((and (zerop code) (gethash lp *ap-form-buttons*))
             (destructuring-bind (f . saved) (gethash lp *ap-form-buttons*) (ap-form-close f saved)) t)
            ((gethash lp *ap-composes*)
             (let ((handle (gethash lp *ap-composes*)))
               (cond ((zerop code) (ap-compose-send handle) t)                                 ; Send
                     ((= code #x300)                                                          ; EN_CHANGE: Enter?
                      (when (find #\Newline (window-text lp)) (ap-compose-send handle))
                      t))))))))

(pushnew 'ap-message-hook *message-hooks*)

;;; ---- the app's folder and the time zone --------------------------------------------------------------------------

(defun ap-documents ()
  "The app's Documents: app-folder\\Documents (so a Library beside it, as an app's, is app-folder\\Library)."
  (let ((dir (concatenate 'string (app-folder) "\\Documents")))
    (c *create-directory* (wide1 dir) 0)
    (substitute #\/ #\\ dir)))

(defun ap-utc-offset ()
  "Seconds east of UTC now (GetTimeZoneInformation: UTC = local + Bias, and the standard or daylight bias)."
  (let* ((tz (modus-int::w32-alloc 172)) (kind (logand (c *ap-get-time-zone* tz) #xFFFFFFFF))
         (bias (+ (s32 (peek32 tz)) (s32 (peek32 (+ tz (if (= kind 2) 168 84)))))))
    (modus-int::w32-free tz 172)
    (* -60 bias)))

;;; ---- the calls -----------------------------------------------------------------------------------------------------

(defswift "app_nav_present" (a n) (ap-nav-present (utf8-text a n)))
(defswift "app_nav_root" (nav) (let ((v (ap-nav-of nav))) (if v (ap-nav-root v) 0)))
(defswift "app_nav_dismiss" (nav) (ap-nav-dismiss nav))
(defswift "app_push" (nav a n) (let ((v (ap-nav-of nav))) (if v (ap-screen-new v (utf8-text a n)) 0)))
(defswift "app_pop" (nav) (let ((v (ap-nav-of nav))) (when v (ap-pop v)) 0))
(defswift "app_set_title" (h a n) (ap-set-title h (utf8-text a n)))
(defswift "app_bar_button" (h a n right) (ap-bar-button h (utf8-text a n) (eql right 1)))
(defswift "app_bar_menu" (h a n) (ap-bar-menu h (term-arg a n)))
(defswift "app_table" (h plain) (ap-table h (eql plain 1)))
(defswift "app_table_set" (table a n) (ap-table-set table (let ((v (term-arg a n))) (if (listp v) v '()))))
(defswift "app_table_empty" (table a n) (ap-table-empty table (term-arg a n)))
(defswift "app_table_scroll_end" (table animated) (ap-table-scroll-end table))
(defswift "app_compose" (h a n) (ap-compose h))
(defswift "app_form" (ta tn sa sn) (ap-form-new (utf8-text ta tn) (utf8-text sa sn)))
(defswift "app_form_field" (f a n) (ap-form-field f (term-arg a n)))
(defswift "app_form_note" (f a n) (ap-form-note f (utf8-text a n)))
(defswift "app_form_show" (f) (ap-form-show f))
(defswift "app_documents" (buf cap) (answer-text (ap-documents) buf cap))
(defswift "app_utc_offset" () (+ 86400 (ap-utc-offset)))
