;;;; zero-display.lisp -- the Zero's screen settings: the desk's resolution, the window manager's
;;;; density, and the margins a TV's overscan needs -- applied when the desk starts, kept on the
;;;; card, and set from a Settings window.
;;;;
;;;; WHAT IS ON THE SCREEN.  The firmware scans out one framebuffer and scales it to the HDMI mode
;;;; it chose at power-on.  modus's console (net/hdmi-console.lisp) allocates that framebuffer 640
;;;; wide in the display's aspect, and the desk draws into it -- so every screen showed 640x400 or
;;;; 640x360 desk pixels, scaled up: coarse on a monitor, and on a TV that overscans, clipped at the
;;;; edges, because config.txt says disable_overscan=1.  The framebuffer can be any size: here it is
;;;; released and allocated again at the size chosen, and the firmware's SET_OVERSCAN tag, sent with
;;;; the allocation, insets the scaled picture by the margins (measured: HVS channel 1's entry
;;;; moves to x 64 y 48, 1792x1104, for margins 48/48/64/64 on a 1920x1200 output).
;;;;
;;;; THE MAILBOX CALL IS OUR OWN.  The console's HCON-MBOX-CALL waits a fixed million polls -- a
;;;; few milliseconds -- which an allocation at boot fits in and one at run time does not (the
;;;; firmware changes the scanout first).  Its answer then arrived late and sat in the read FIFO,
;;;; where the NEXT call took it for its own: every live allocation came back as the request echoed
;;;; (address 16, pitch 0).  %CALL empties the FIFO first and waits up to a second of real time.
;;;; An allocation is believed only if it is plausible (inside the GPU's memory, pitch = 4 x width);
;;;; otherwise the console is left on what it had.
;;;;
;;;; DENSITY is glass.wm's *WM-SCALE* (the chrome's design pixels per device pixel).  It is read
;;;; when the desk is made -- windows keep the geometry they were opened with -- so a change takes
;;;; a restart of the desk, not of the board: KILN-ZERO-MAIN loops on RESTART-DESK.
;;;;
;;;; A CHANGE MUST BE KEPT.  A resolution or margin the screen cannot show leaves you nothing to
;;;; click on.  So Apply saves the new settings as PENDING beside the old ones and restarts the
;;;; desk; the Settings window opens counting down, and unless Keep is pressed within 15 s the old
;;;; settings come back (as a monitor's own menu does).  At start, a size the firmware refuses
;;;; falls back to the console's own 640-wide one.
;;;;
;;;; THE FILE is /display.txt on the card's boot partition (kiln-zero-card), one line:
;;;;   WIDTH HEIGHT DENSITY TOP BOTTOM LEFT RIGHT [PENDING W H D T B L R]
;;;; No file: the desk as it always was (the console's size, density 2).

(defpackage #:kiln-zero-display
  (:use #:cl)
  (:export #:display-start #:display-info #:resolutions #:settings #:settings-app
           #:restart-desk #:*restart*))

(in-package #:kiln-zero-display)

;;; The console's and the HDMI driver's functions are the KERNEL's: called by name here, they
;;; compile to direct calls.  (Looked up with FDEFINITION they are not there -- most kernel
;;; functions have no entry in the runtime's function table -- which is what a first cut did.)
(defun %wr (a v) (modus-int::hdmi-wr a v))
(defun %rd (a) (modus-int::hdmi-rd a))
(defun %buf () (modus-int::hdmi-mbox-buf))
(defun %hget (off) (modus-int::hcon-get off))
(defun %hput (off v) (modus-int::hcon-put off v))

;;; ---- the mailbox ---------------------------------------------------------------------------

(defun %clear (n)
  (let ((b (%buf))) (loop for j from 0 below n by 4 do (%wr (+ b j) 0))))

(defun %drain ()
  "Empty the read FIFO of answers nobody waited for; how many there were."
  (let ((st (modus-int::hdmi-mbox-status)) (rd (modus-int::hdmi-mbox-read)))
    (loop for n from 0 to 64
          when (logtest (%rd st) #x40000000) return n
          do (%rd rd))))

(defun %call ()
  "Send the property buffer on channel 8 and wait for its answer: T, or NIL if none came in 1 s."
  (%drain)
  (let ((base (modus-int::hdmi-mbox-base)) (st (modus-int::hdmi-mbox-status))
        (limit (floor internal-time-units-per-second 1)))
    (let ((t0 (get-internal-real-time)))
      (loop until (zerop (logand (%rd (+ base #x38)) #x80000000))
            when (> (- (get-internal-real-time) t0) limit) return nil))
    (%wr (modus-int::hdmi-mbox-write) (logior (modus-int::hdmi-mbox-buf-bus) 8))
    (let ((t0 (get-internal-real-time)))
      (loop (unless (logtest (%rd st) #x40000000)
              (%rd (modus-int::hdmi-mbox-read))
              (return (= (%rd (+ (%buf) 4)) #x80000000)))
            (when (> (- (get-internal-real-time) t0) limit) (return nil))))))

(defun %release ()
  (let ((b (%buf)))
    (%clear 32)
    (%wr b 32) (%wr (+ b 8) #x48001) (%wr (+ b 12) 0)
    (%call)))

(defun %alloc (w h top bottom left right)
  "Release the scanout and allocate W x H at 32 bpp, inset by the margins.  (values FB PITCH),
   or NIL when the firmware refused or answered something implausible."
  (%release)
  (let ((b (%buf)))
    (%clear 160)
    (%wr b 160)
    (%wr (+ b 8) #x48003) (%wr (+ b 12) 8) (%wr (+ b 20) w) (%wr (+ b 24) h)       ; physical
    (%wr (+ b 28) #x48004) (%wr (+ b 32) 8) (%wr (+ b 40) w) (%wr (+ b 44) h)      ; virtual
    (%wr (+ b 48) #x48005) (%wr (+ b 52) 4) (%wr (+ b 60) 32)                       ; depth
    (%wr (+ b 64) #x4800A) (%wr (+ b 68) 16)                                        ; overscan
    (%wr (+ b 76) top) (%wr (+ b 80) bottom) (%wr (+ b 84) left) (%wr (+ b 88) right)
    (%wr (+ b 92) #x40001) (%wr (+ b 96) 8) (%wr (+ b 104) 16)                      ; allocate
    (%wr (+ b 112) #x40008) (%wr (+ b 116) 4)                                       ; pitch
    (when (%call)
      (let ((fb (logand (%rd (+ b 104)) #x3FFFFFFF)) (pitch (%rd (+ b 124))))
        (when (and (>= fb #x1C000000) (< fb #x20000000) (= pitch (* 4 w)))
          (values fb pitch))))))

(defun %adopt (w h fb pitch)
  "Make FB (W x H) the console's framebuffer: mapped non-cacheable, its geometry recorded where
   the console, the desk's blit and the screenshot code read it."
  (modus-int::hcon-map-nc fb (* h pitch))
  (%hput #x04 fb) (%hput #x08 pitch) (%hput #x0C w) (%hput #x10 h)
  (%hput #x14 (floor w 8)) (%hput #x18 (floor h 8)) (%hput #x1C 0) (%hput #x20 0)
  (let ((st (modus-int::hdmi-fb-state)))
    (%wr st fb) (%wr (+ st 8) pitch) (%wr (+ st 12) w) (%wr (+ st 16) h))
  (modus-int::hcon-nfill fb (* h pitch) (%hget #x28))
  t)

(defun %set-screen (w h margins)
  "The scanout at W x H with MARGINS (top bottom left right); else the console's own size.
   Returns the size in use as (W H)."
  (multiple-value-bind (fb pitch) (apply #'%alloc w h margins)
    (if fb
        (progn (%adopt w h fb pitch) (list w h))
        (destructuring-bind (cw ch) (console-size)
          (multiple-value-bind (fb2 pitch2) (%alloc cw ch 0 0 0 0)
            (when fb2 (%adopt cw ch fb2 pitch2)))
          (list cw ch)))))

;;; ---- what the screen is --------------------------------------------------------------------

(defun output-size ()
  "The HDMI output's size, from the HVS channel the firmware drives (DISPCTRL1: width 23:12,
   height 11:0) -- the monitor's mode, not the framebuffer's."
  (let ((ctl (cl-user::mem-ref (+ #x3F400000 #x50) :u32)))
    (let ((w (logand (ash ctl -12) 4095)) (h (logand ctl 4095)))
      (if (and (plusp w) (plusp h)) (list w h) (list 1920 1080)))))

(defun console-size ()
  "The size the console picks for this output: 640 wide in its aspect (hdmi-console.lisp)."
  (destructuring-bind (w h) (output-size)
    (list 640 (min 480 (max 200 (logand (floor (* 640 h) w) (lognot 7)))))))

(defun edid ()
  "EDID block 0 (128 bytes), or NIL."
  (let ((b (%buf)))
    (%clear 160)
    (%wr b 160) (%wr (+ b 8) #x30020) (%wr (+ b 12) 136) (%wr (+ b 20) 0)
    (when (%call)
      ;; the data's offset in the answer is found by its header, 00 FF FF FF FF FF FF 00
      (let ((bytes (make-array 140 :element-type '(unsigned-byte 8))))
        (dotimes (i 35)
          (let ((w (%rd (+ b 20 (* 4 i)))))
            (dotimes (k 4) (setf (aref bytes (+ k (* 4 i))) (logand (ash w (* -8 k)) 255)))))
        (let ((at (search #(0 255 255 255 255 255 255 0) bytes)))
          (and at (<= (+ at 128) 140) (subseq bytes at (+ at 128))))))))

(defun %edid-name (e)
  (loop for d in '(54 72 90 108)
        when (and (zerop (aref e d)) (zerop (aref e (1+ d))) (= (aref e (+ d 3)) #xFC))
          return (string-trim '(#\Space #\Newline)
                              (map 'string #'code-char
                                   (subseq e (+ d 5) (or (position 10 e :start (+ d 5) :end (+ d 18))
                                                         (+ d 18)))))))

(defun %edid-modes (e)
  "The modes the screen lists: its detailed timings (the first is its preferred mode), then its
   standard timings, as (W H)."
  (let ((out '()))
    (dolist (d '(54 72 90 108))
      (unless (and (zerop (aref e d)) (zerop (aref e (1+ d))))
        (push (list (+ (aref e (+ d 2)) (ash (ash (aref e (+ d 4)) -4) 8))
                    (+ (aref e (+ d 5)) (ash (ash (aref e (+ d 7)) -4) 8)))
              out)))
    (loop for i from 38 below 54 by 2
          unless (and (= (aref e i) 1) (= (aref e (1+ i)) 1))
            do (let* ((w (* 8 (+ (aref e i) 31)))
                      (h (case (ash (aref e (1+ i)) -6)
                           (0 (floor (* w 10) 16)) (1 (floor (* w 3) 4))
                           (2 (floor (* w 4) 5)) (t (floor (* w 9) 16)))))
                 (push (list w h) out)))
    (remove-duplicates (nreverse out) :test #'equal :from-end t)))

(defun display-info ()
  "(:name NAME :output (W H) :preferred (W H) :modes ((W H) ...)) for the screen attached."
  (let ((e (ignore-errors (edid))))
    (list :name (and e (%edid-name e)) :output (output-size)
          :preferred (and e (first (%edid-modes e)))
          :modes (and e (%edid-modes e)))))

(defun resolutions ()
  "What the desk can be, largest first: the output's own size (one desk pixel per screen pixel),
   fractions of it that keep its shape, and the console's 640-wide size."
  (destructuring-bind (w h) (output-size)
    (let ((out '()))
      (dolist (f '((1 1) (3 4) (2 3) (1 2) (1 3)))
        (let ((fw (* 8 (round (* w (first f)) (* 8 (second f)))))
              (fh (* 8 (round (* h (first f)) (* 8 (second f))))))
          (when (>= fw 480) (pushnew (list fw fh) out :test #'equal))))
      (pushnew (console-size) out :test #'equal)
      (sort out #'> :key #'first))))

;;; ---- the settings, on the card -------------------------------------------------------------

(defparameter *file* "display.txt")
(defvar *restart* nil "T while the desk is being restarted for new settings.")
(defvar *pending-since* nil "GET-INTERNAL-REAL-TIME when pending settings were applied, or NIL.")
(defparameter *keep-seconds* 15)

(defun default-settings () (append (console-size) (list 2 0 0 0 0)))

(defun %card (name &rest args)
  (let ((s (find-symbol name "KILN-ZERO-CARD")))
    (and s (fboundp s) (ignore-errors (apply s args)))))

(defun %words (text)
  (let ((out '()) (start nil) (text (or text "")))
    (dotimes (i (length text))
      (if (member (char text i) '(#\Space #\Tab #\Newline #\Return))
          (when start (push (subseq text start i) out) (setq start nil))
          (unless start (setq start i))))
    (when start (push (subseq text start) out))
    (nreverse out)))

(defun %parse (text)
  "(values CURRENT PENDING) from the file's words; either NIL when it is not a valid setting."
  (flet ((nums (words) (mapcar (lambda (w) (ignore-errors (parse-integer w))) words))
         (ok (l) (and (= (length l) 7) (every #'integerp l)
                      (< 0 (first l) 4097) (< 0 (second l) 4097) (<= 1 (third l) 4)
                      (every (lambda (m) (<= 0 m 400)) (nthcdr 3 l)))))
    (let* ((words (%words text))
           (cur (nums (subseq words 0 (min 7 (length words)))))
           (pend (and (> (length words) 7) (string-equal (nth 7 words) "PENDING")
                      (nums (subseq words 8 (min 15 (length words)))))))
      (values (and (ok cur) cur) (and pend (ok pend) pend)))))

(defun settings ()
  "(values CURRENT PENDING): the settings in use, and those being tried (or NIL)."
  (multiple-value-bind (cur pend) (%parse (%card "CARD-TEXT" *file*))
    (values (or cur (default-settings)) pend)))

(defun %save (cur &optional pend)
  (%card "CARD-WRITE" *file*
         (format nil "~{~D~^ ~}~@[ PENDING~{ ~D~}~]~%" cur pend)))

;;; ---- applying them -------------------------------------------------------------------------

(defun %wm-scale (d)
  (let ((s (find-symbol "*KILN-WM-SCALE*" "COMMON-LISP-USER")))
    (when s (setf (symbol-value s) d))))

(defvar *applied* nil
  "The size and margins the scanout was last set to here, (W H TOP BOTTOM LEFT RIGHT); NIL until
   then, when it is the console's own size with no margins.")

(defun display-start ()
  "Before the desk is made: the scanout at the saved (or pending) size and margins, and the
   window manager's density.  Called by KILN-ZERO-MAIN each time it starts the desk."
  (multiple-value-bind (cur pend) (settings)
    (let ((use (or pend cur)))
      (destructuring-bind (w h d top bottom left right) use
        (let ((want (list w h top bottom left right))
              (have (or *applied* (append (list (%hget #x0C) (%hget #x10)) (list 0 0 0 0)))))
          (unless (equal want have)
            (let ((got (%set-screen w h (list top bottom left right))))
              (setf *applied* (if (equal got (list w h)) want (append got (list 0 0 0 0)))))))
        (%wm-scale d)
        (setf *pending-since* (and pend (get-internal-real-time)))
        use))))

(defun restart-desk ()
  "Leave the desk and start it again (KILN-ZERO-MAIN loops), at whatever DISPLAY-START finds."
  (setf *restart* t)
  (throw (find-symbol "KILN-ZERO-RESTART" "COMMON-LISP-USER") nil))

(defun apply-settings (new)
  "Try NEW: saved as pending beside what is in use, the desk restarted on it."
  (let ((cur (settings)))
    (%save cur new)
    (restart-desk)))

(defun keep-settings ()
  (multiple-value-bind (cur pend) (settings)
    (declare (ignore cur))
    (when pend (%save pend))
    (setf *pending-since* nil)))

(defun revert-settings ()
  (%save (settings))
  (setf *pending-since* nil)
  (restart-desk))

(defun pending-left ()
  "Seconds left to keep pending settings, or NIL when nothing is pending."
  (when *pending-since*
    (max 0 (- *keep-seconds*
              (floor (- (get-internal-real-time) *pending-since*) internal-time-units-per-second)))))

(defun %frame-hook ()
  "Run every frame: pending settings not kept in time are put back."
  (let ((left (pending-left)))
    (when (and left (<= left 0)) (revert-settings))))

;;; ---- the Settings window -------------------------------------------------------------------

(defparameter +bg+ #x10141a)
(defparameter +paper+ #x1c222b)
(defparameter +ink+ #xe6ebf2)
(defparameter +dim+ #x8b949e)
(defparameter +button+ #x2f3b4d)
(defparameter +accent+ #x5abe82)
(defparameter +warn+ #xe0a800)

(defun settings-app (fb)
  "The Settings window: the screen, its resolution, density and margins.  A desk make-fn."
  (multiple-value-bind (cur pend) (settings)
    (let* ((info (display-info))
           (choice (copy-list (or pend cur)))
           (hits '()) (dirty t) (was nil) (shown-left nil))
      (labels ((text (x y s &key (size 14) (color +ink+) bold)
                 (glass:fb-text fb x y s :size size :color color :font (glass:default-font bold)))
               (button (x y w h label kind &key on (size 14))
                 (glass:fb-rect fb x y w h (if on +accent+ +button+))
                 (text (+ x 10) (+ y (floor (- h size) 2) -1) label :size size :color (if on +bg+ +ink+))
                 (push (list kind x y w h) hits))
               (redraw ()
                 ;; compact: each label beside its controls, so the window fits a 640x400 desk
                 (setf hits '())
                 (glass:fb-fill fb +bg+)
                 (let ((y 10) (w (glass:fb-width fb)) (cx 112))
                   (text 12 y "Display" :size 15 :bold t)
                   (text cx (+ y 2) (format nil "~A, output ~{~D x ~D~}"
                                            (or (getf info :name) "screen") (getf info :output))
                         :size 12 :color +dim+)
                   (incf y 28)
                   ;; resolution: as many buttons a row as fit beside the label
                   (text 12 (+ y 5) "Resolution" :size 13 :bold t)
                   (let ((x cx))
                     (dolist (r (resolutions))
                       (when (> (+ x 104) w) (setf x cx) (incf y 28))
                       (button x y 98 24 (format nil "~D x ~D" (first r) (second r)) (list :res r)
                               :on (equal r (subseq choice 0 2)) :size 12)
                       (incf x 104)))
                   (incf y 32)
                   ;; density
                   (text 12 (+ y 5) "Chrome" :size 13 :bold t)
                   (loop for d in '(1 2 3) for x from cx by 52
                         do (button x y 46 24 (format nil "~Dx" d) (list :density d)
                                    :on (= d (third choice)) :size 12))
                   (incf y 32)
                   ;; margins, two to a row
                   (text 12 (+ y 5) "Margins" :size 13 :bold t)
                   (loop for name in '("Top" "Bottom" "Left" "Right") for i from 3
                         for x = (if (evenp (- i 3)) cx (+ cx 172))
                         do (text x (+ y 5) name :size 12 :color +dim+)
                            (button (+ x 52) y 26 24 "-" (list :margin i -8) :size 12)
                            (text (+ x 84) (+ y 5) (format nil "~D" (nth i choice)) :size 12)
                            (button (+ x 112) y 26 24 "+" (list :margin i 8) :size 12)
                            (when (oddp (- i 3)) (incf y 28)))
                   (text cx (+ y 2) "for a TV that cuts off the edges" :size 11 :color +dim+)
                   (incf y 24)
                   ;; apply, or keep what is being tried
                   (let ((left (pending-left)))
                     (if left
                         (progn
                           (button 12 y 80 28 "Keep" (list :keep) :on t)
                           (button 100 y 80 28 "Go back" (list :revert))
                           (text 192 (+ y 7) (format nil "Going back in ~D s" left)
                                 :size 13 :color +warn+ :bold t))
                         (button 12 y 80 28 "Apply" (list :apply)
                                 :on (not (equal choice (settings)))))))
                 (setf shown-left (pending-left)))
               (press (x y)
                 (let ((h (find-if (lambda (h) (destructuring-bind (k hx hy hw hh) h
                                                 (declare (ignore k))
                                                 (and (<= hx x (+ hx hw)) (<= hy y (+ hy hh)))))
                                   hits)))
                   (when h
                     (let ((k (first h)))
                       (case (first k)
                         (:res (setf (first choice) (first (second k)) (second choice) (second (second k))))
                         (:density (setf (third choice) (second k)))
                         (:margin (setf (nth (second k) choice)
                                        (max 0 (min 200 (+ (nth (second k) choice) (third k))))))
                         (:apply (unless (equal choice (settings)) (apply-settings (copy-list choice))))
                         (:keep (keep-settings))
                         (:revert (revert-settings))))
                     (setf dirty t)))))
        (values nil
                (lambda (mask x y)
                  (let ((now (logbitp 0 mask)))
                    (when (and now (not was)) (press x y))
                    (setf was now)))
                (lambda ()
                  (unless (eql (pending-left) shown-left) (setf dirty t))
                  (when dirty (setf dirty nil) (redraw) t))
                nil nil
                (lambda (w h) (glass:fb-resize fb w h +bg+) (setf dirty t))
                nil)))))

;;; ---- on the desk ---------------------------------------------------------------------------

(defun %on-desk (desk dw dh)
  "app.lisp's desk-setup hook: the Settings window in the menu, the keep-or-go-back watch as a
   frame hook, and the window opened at once when settings are being tried."
  (funcall (find-symbol "%KILN-DESK-REGISTER-APP" "COMMON-LISP-USER") desk "Settings"
           #'settings-app :width (min dw 480) :height (min (- dh 60) 290))
  (let ((hooks (find-symbol "*KILN-FRAME-HOOKS*" "COMMON-LISP-USER")))
    (pushnew '%frame-hook (symbol-value hooks)))
  (when *pending-since*
    (funcall (find-symbol "%KILN-DESK-OPEN" "COMMON-LISP-USER") desk "Settings")))

(let ((s (find-symbol "*KILN-DESK-SETUP-HOOKS*" "COMMON-LISP-USER")))
  (when s (pushnew '%on-desk (symbol-value s))))
