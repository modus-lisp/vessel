;;;; win-ui-gestures.lisp -- the gestures, menus and effects of the Windows host: what the Apple host does with UIKit
;;;; gesture recognizers, animations, menus and the keyboard, made here on Win32.  Loads after win-host.lisp.
;;;;
;;;; Gestures are found by POLLING, once a frame in a tick hook (GS-TICK): a view's own window procedure does not
;;;; see the mouse, so no message says a tap or a drag happened.  The pointer is read with GetCursorPos, the buttons
;;;; with GetAsyncKeyState.  Each gesture is a recognizer (GREC) whose state machine is GREC-FEED: one sample at a
;;;; time -- (down x y time) in pixels and milliseconds, with the target's rectangle -- so tests can feed it directly.
;;;;
;;;; Calls implemented here (the Swift names of ios-swift.lisp):
;;;;   ui_tap_on            a tap (not on a button: its WM_COMMAND already says so)        (:tap h)
;;;;   ui_long_press_on     held MS ms without moving                                       (:longpress h)
;;;;   ui_swipe_on          a fast move released, mostly in one direction                   (:swipe h dir)
;;;;   ui_pan_on            a drag, reported when released, in points                       (:pan h dx)
;;;;   ui_edge_pan_on       a drag from the window's left edge across the view              (:edge h)
;;;;   ui_context_menu_on   a right click, or a left long press, pops a TrackPopupMenu       (:context h index)
;;;;   ui_edit_menu_on      a left long press pops a TrackPopupMenu                         (:edit h index)
;;;;   ui_animate_to        the frame moves, eased, a step each frame (SetWindowPos)
;;;;   ui_fade_to           the alpha moves (WS_EX_LAYERED + SetLayeredWindowAttributes)    (:fade h)
;;;;   ui_gravity_drop      the view falls to its parent's client bottom, accelerating
;;;;   ui_keyboard_avoid_on / ui_keyboard_post   there is no keyboard here: the views move up HEIGHT points, back at 0
;;;;   ui_input_keys_on     a row of buttons under a text field; a click inserts the key (EM_REPLACESEL)
;;;;   ui_announce          printed                                    ui_set_accessibility  remembered (no caption change)
;;;;   ui_parallax_on, ui_haptic   no-ops (no tilt, no haptics on a PC)
;;;;
;;;; Not done: a GPU view (no window of its own) is not hit-tested, so gestures on one never fire.
(in-package #:vessel-windows)

;;; ---- the Win32 calls not in win-host's DEFPROCS -------------------------------------------------------------------

(defvar *get-async-key-state* (modus-int::w32-proc "user32.dll" "GetAsyncKeyState"))
(defvar *get-window-rect* (modus-int::w32-proc "user32.dll" "GetWindowRect"))
(defvar *get-parent* (modus-int::w32-proc "user32.dll" "GetParent"))
(defvar *destroy-menu* (modus-int::w32-proc "user32.dll" "DestroyMenu"))
(defvar *get-menu-item-count* (modus-int::w32-proc "user32.dll" "GetMenuItemCount"))
(defvar *get-window-long-ptr* (modus-int::w32-proc "user32.dll" "GetWindowLongPtrW"))
(defvar *set-window-long-ptr* (modus-int::w32-proc "user32.dll" "SetWindowLongPtrW"))
(defvar *set-layered-attributes* (modus-int::w32-proc "user32.dll" "SetLayeredWindowAttributes"))
(defvar *is-window-visible* (modus-int::w32-proc "user32.dll" "IsWindowVisible"))

;;; A scratch block of our own (the call frame's areas belong to win-host): +0 a RECT, +32 a POINT.

(defvar *gs-buf* 0)

(defun gs-buf ()
  (when (zerop *gs-buf*) (setf *gs-buf* (modus-int::w32-alloc 256 t)))
  *gs-buf*)

(defun gs-now ()
  "The time in milliseconds."
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun gs-points (pixels)
  "A distance in pixels, in points."
  (round (* pixels 96) *dpi*))

(defun gs-clamp (v lo hi) (max lo (min hi v)))

(defun gs-screen-rect (hwnd)
  "HWND's rectangle in screen pixels: (left top right bottom); NIL for no window."
  (unless (zerop hwnd)
    (let ((b (gs-buf)))
      (c *get-window-rect* hwnd b)
      (list (s32 (peek32 b)) (s32 (peek32 (+ b 4))) (s32 (peek32 (+ b 8))) (s32 (peek32 (+ b 12)))))))

(defun gs-frame (hwnd)
  "HWND's frame in pixels, relative to its parent's client area: (x y w h); NIL for no window."
  (let ((rect (gs-screen-rect hwnd)))
    (when rect
      (let* ((b (gs-buf)) (parent (c *get-parent* hwnd)))
        (destructuring-bind (l tp r bt) rect
          (poke32 b l) (poke32 (+ b 4) tp) (poke32 (+ b 8) r) (poke32 (+ b 12) bt)
          (unless (zerop parent) (c *map-points* 0 parent b 2))
          (let ((x (s32 (peek32 b))) (y (s32 (peek32 (+ b 4)))))
            (list x y (- (s32 (peek32 (+ b 8))) x) (- (s32 (peek32 (+ b 12))) y))))))))

(defun gs-client-size (hwnd)
  "HWND's client area, pixels: (values width height)."
  (let ((b (gs-buf)))
    (c *get-client-rect* hwnd b)
    (values (s32 (peek32 (+ b 8))) (s32 (peek32 (+ b 12))))))

(defun gs-place (hwnd x y &optional w h)
  "Move HWND to X Y (pixels, in its parent) and size it W H if given: on the windows' thread."
  (if (and w h)
      (u *set-window-pos* hwnd 0 x y w h #x14)            ; SWP_NOZORDER | SWP_NOACTIVATE
      (u *set-window-pos* hwnd 0 x y 0 0 #x15)))          ; and SWP_NOSIZE

(defun gs-button-down (vk)
  (logtest (logand (c *get-async-key-state* vk) #xFFFF) #x8000))

;;; ---- gestures: the recognizers -------------------------------------------------------------------------------------

(defstruct grec
  kind                       ; :tap :longpress :swipe :pan :edge
  h                          ; the view
  (button :left)             ; :left or :right
  (ms 500)                   ; a long press's hold
  (dir 0)                    ; a swipe's direction: 0 left, 1 right, 2 up, 3 down
  fire                       ; (lambda (arg)): what the gesture does when it is complete; ARG is pan's dx
  (state :idle)              ; :idle or :down
  (t0 0) (x0 0) (y0 0) (x 0) (y 0)
  (maxd 0)                   ; the farthest the pointer went from where the press began, pixels (chebyshev)
  (touched nil)              ; the pointer has been over the view during this press (edge pan)
  (fired nil))               ; a long press has fired

(defvar *gs-recs* '() "Every recognizer, oldest first.")
(defvar *gs-menu-open* nil "A popup menu is up: no gesture is taken meanwhile.")

(defun gs-add (r)
  (setf *gs-recs* (append *gs-recs* (list r)))
  r)

(defun gs-slop () (px 10))              ; movement that is not a tap, or a long press that is still held still
(defun gs-tap-ms () 500)
(defun gs-swipe-ms () 500)
(defun gs-swipe-min () (px 40))         ; a swipe's least length
(defun gs-edge-band () (px 20))         ; a pan from the edge starts within this of the window's left edge

(defun gs-inside (rect x y)
  (and rect (<= (first rect) x) (< x (third rect)) (<= (second rect) y) (< y (fourth rect))))

(defun gs-starts-p (r x y rect win-left)
  "Does a press at X Y begin this gesture?"
  (if (eq (grec-kind r) :edge)
      (and win-left (<= 0 (- x win-left)) (< (- x win-left) (gs-edge-band)))
      (gs-inside rect x y)))

(defun gs-swipe-p (dir dx dy)
  "Is the move DX DY (pixels) a swipe in direction DIR: long enough, and mostly that way."
  (let ((m (gs-swipe-min)))
    (case dir
      (0 (and (<= dx (- m)) (>= (- dx) (* 2 (abs dy)))))
      (1 (and (>= dx m) (>= dx (* 2 (abs dy)))))
      (2 (and (<= dy (- m)) (>= (- dy) (* 2 (abs dx)))))
      (3 (and (>= dy m) (>= dy (* 2 (abs dx)))))
      (t nil))))

(defun gs-feed (r down x y now rect win-left)
  "One sample for the recognizer R: whether its button is DOWN, the pointer at X Y (pixels), the time NOW (ms), the
   target view's RECT (screen pixels, or NIL), and the main window's left edge WIN-LEFT (screen pixels).  When a
   gesture is complete, its event is sent."
  (cond
    ((eq (grec-state r) :idle)
     (when (and down (gs-starts-p r x y rect win-left))
       (setf (grec-state r) :down
             (grec-t0 r) now (grec-x0 r) x (grec-y0 r) y (grec-x r) x (grec-y r) y
             (grec-maxd r) 0 (grec-fired r) nil
             (grec-touched r) (gs-inside rect x y))))
    (down
     (setf (grec-x r) x (grec-y r) y)
     (setf (grec-maxd r) (max (grec-maxd r)
                              (abs (- x (grec-x0 r))) (abs (- y (grec-y0 r)))))
     (when (gs-inside rect x y) (setf (grec-touched r) t))
     (when (and (eq (grec-kind r) :longpress) (not (grec-fired r))
                (<= (grec-maxd r) (gs-slop))
                (>= (- now (grec-t0 r)) (grec-ms r)))
       (setf (grec-fired r) t)
       (funcall (grec-fire r) nil)))
    (t                                                   ; released: the press is over
     (let* ((kind (grec-kind r))
            (dur (- now (grec-t0 r)))
            (dx (- x (grec-x0 r)))
            (dy (- y (grec-y0 r))))
       (cond
         ((eq kind :tap)
          (when (and (<= (grec-maxd r) (gs-slop)) (< dur (gs-tap-ms)))
            (funcall (grec-fire r) nil)))
         ((eq kind :swipe)
          (when (and (<= dur (gs-swipe-ms)) (gs-swipe-p (grec-dir r) dx dy))
            (funcall (grec-fire r) nil)))
         ((eq kind :pan)
          (when (> (grec-maxd r) (gs-slop))
            (funcall (grec-fire r) (gs-points dx))))
         ((eq kind :edge)
          (when (and (grec-touched r) (> dx (gs-slop)))
            (funcall (grec-fire r) nil))))
       (setf (grec-state r) :idle (grec-fired r) nil)))))

;;; ---- the tick: sample every recognizer ---------------------------------------------------------------------------------

(defun gs-view-rect (h)
  "The screen rectangle of the view H, or NIL: no window (a GPU view), or no such handle."
  (let ((ctl (gethash h *ctls*)))
    (and ctl (gs-screen-rect (ctl-hwnd ctl)))))

(defun gs-tick ()
  "Once a frame: the animations step, and each recognizer takes the pointer's sample."
  (gs-run-anims)
  (when (and *gs-recs* (not *gs-menu-open*) (plusp *hwnd*))
    (let* ((b (gs-buf)) (now (gs-now)))
      (c *get-cursor-pos* (+ b 32))
      (let ((x (s32 (peek32 (+ b 32)))) (y (s32 (peek32 (+ b 36))))
            (left (gs-button-down 1)) (right (gs-button-down 2))
            (win-left (first (gs-screen-rect *hwnd*))))
        (dolist (r *gs-recs*)
          (unless *gs-menu-open*
            (when (gethash (grec-h r) *ctls*)
              (gs-feed r (if (eq (grec-button r) :right) right left) x y now
                       (gs-view-rect (grec-h r)) win-left))))))))

(pushnew 'gs-tick *tick-hooks*)

;;; ---- the menus ---------------------------------------------------------------------------------------------------------

(defun gs-strings (term)
  (mapcar (lambda (x) (if (stringp x) x (princ-to-string x))) (if (listp term) term nil)))

(defun gs-make-menu (choices)
  "A popup menu of the strings CHOICES, command ids 1 to n."
  (let ((menu (c *create-popup-menu*)))
    (loop for s in choices for i from 1
          do (c *append-menu* menu 0 i (wide1 s)))                  ; MF_STRING
    menu))

(defun gs-menu-done (h kind menu id)
  "The menu MENU, popped for H, returned ID (0: none chosen): the event is (KIND H index), index from 0."
  (unless (zerop menu) (c *destroy-menu* menu))
  (setf *gs-menu-open* nil)
  (let ((sel (logand id #xFFFFFFFF)))
    (when (plusp sel) (ui-event (list kind h (1- sel))))))

(defun gs-pop-menu (h kind choices)
  "Pop the menu of CHOICES at the pointer (TrackPopupMenu, TPM_RETURNCMD | TPM_RIGHTBUTTON), on the windows' thread:
   its answer comes later, to GS-MENU-DONE."
  (let ((menu (gs-make-menu choices)) (b (gs-buf)))
    (c *get-cursor-pos* (+ b 32))
    (let ((x (s32 (peek32 (+ b 32)))) (y (s32 (peek32 (+ b 36)))))
      (dolist (r *gs-recs*) (setf (grec-state r) :idle))     ; no press is taken across a menu
      (setf *gs-menu-open* t)
      (u *set-foreground* *hwnd*)
      (ui-post *track-popup-menu* (list menu #x102 x y 0 *hwnd* 0)
               (lambda (id mem)
                 (declare (ignore mem))
                 (gs-menu-done h kind menu id))))))

;;; ---- the animations --------------------------------------------------------------------------------------------------

(defstruct gsanim
  kind            ; :frame (from/to: (x y w h)), :alpha (from/to: 0-255), :fall (from: (x y0), to: target y)
  h hwnd
  (t0 0) (ms 1)   ; started at T0 (ms), lasting MS
  plist)          ; :from and :to, as above

(defvar *gs-anims* '())
(defvar *gs-alpha* (make-hash-table) "A layered window's alpha, 0-255, by hwnd.")
(defparameter *gs-gravity* 1600 "Gravity, points per second squared.")

(defun gs-remove-anims (hwnd kind)
  (setf *gs-anims* (remove-if (lambda (a) (and (eql (gsanim-hwnd a) hwnd) (eq (gsanim-kind a) kind))) *gs-anims*)))

(defvar *gs-layered* (make-hash-table) "hwnd -> true when its WS_EX_LAYERED took (see GS-ENSURE-LAYERED).")

(defun gs-set-alpha (hwnd a)
  (setf (gethash hwnd *gs-alpha*) a)
  (when (gethash hwnd *gs-layered*)
    (u *set-layered-attributes* hwnd 0 a 2)))                       ; LWA_ALPHA

(defun gs-ensure-layered (hwnd)
  "Make HWND layered (WS_EX_LAYERED).  True if it took: on this Windows, a child window refuses the style, so the
   fade falls back to showing and hiding the view (GS-STEP)."
  (let ((ex (logand (c *get-window-long-ptr* hwnd -20) #xFFFFFFFF)))   ; GWL_EXSTYLE
    (unless (logtest ex #x80000)                                       ; WS_EX_LAYERED
      (u *set-window-long-ptr* hwnd -20 (logior ex #x80000))))
  (setf (gethash hwnd *gs-layered*)
        (logtest (logand (c *get-window-long-ptr* hwnd -20) #xFFFFFFFF) #x80000)))

(defun gs-step (a now)
  "Move the animation A to where it is at NOW (ms).  True when it is complete."
  (let* ((ms (gsanim-ms a))
         (done (>= (- now (gsanim-t0 a)) ms))
         (f (if done 1.0 (/ (float (- now (gsanim-t0 a))) (float ms))))
         (hw (gsanim-hwnd a)))
    (ecase (gsanim-kind a)
      (:frame
       (let ((e (let ((g (- 1.0 f))) (- 1.0 (* g g g)))))          ; ease out, cubic
         (destructuring-bind (x0 y0 w0 h0) (getf (gsanim-plist a) :from)
           (destructuring-bind (x1 y1 w1 h1) (getf (gsanim-plist a) :to)
             (flet ((at (p q) (round (+ p (* (- q p) e)))))
               (gs-place hw (at x0 x1) (at y0 y1) (at w0 w1) (at h0 h1)))))))
      (:fall
       (destructuring-bind (x y0) (getf (gsanim-plist a) :from)
         (let ((dist (- (getf (gsanim-plist a) :to) y0)))
           (gs-place hw x (round (+ y0 (* dist f f)))))))              ; accelerating: quadratic in time
      (:alpha
       (let ((from (getf (gsanim-plist a) :from)) (to (getf (gsanim-plist a) :to)))
         (gs-set-alpha hw (round (+ from (* (- to from) f))))
         (when done
           ;; not layered: the fade is only shown at its two ends, as a view hidden at 0 and shown otherwise
           (unless (gethash hw *gs-layered*)
             (u *show-window* hw (if (plusp to) 5 0)))
           (ui-event (list :fade (gsanim-h a)))))))
    done))

(defun gs-run-anims ()
  (when *gs-anims*
    (let ((now (gs-now)))
      (setf *gs-anims* (remove-if (lambda (a) (gs-step a now)) *gs-anims*)))))

(defun gs-animate (kind h hwnd ms &rest plist)
  "Start an animation of KIND on H's window HWND, lasting MS; PLIST holds :from and :to."
  (push (make-gsanim :kind kind :h h :hwnd hwnd :t0 (gs-now) :ms (max 1 ms) :plist plist) *gs-anims*))

(defswift "ui_animate_to" (h x y w hh ms)
  (let* ((hw (ctl-hwnd (ctl-of h))) (cur (gs-frame hw)))
    (unless (zerop hw)
      (gs-remove-anims hw :frame)
      (if (or (null cur) (<= ms 0))
          (gs-place hw (px x) (px y) (px w) (px hh))
          (gs-animate :frame h hw ms :from cur :to (list (px x) (px y) (px w) (px hh))))))
  0)

(defswift "ui_fade_to" (h percent ms)
  (let* ((hw (ctl-hwnd (ctl-of h)))
         (to (gs-clamp (round (* percent 255) 100) 0 255)))
    (unless (zerop hw)
      (unless (gethash hw *gs-alpha*)
        (gs-ensure-layered hw)
        (gs-set-alpha hw 255))
      (gs-remove-anims hw :alpha)
      (if (<= ms 0)
          (progn (gs-set-alpha hw to)
                 (unless (gethash hw *gs-layered*) (u *show-window* hw (if (plusp to) 5 0)))
                 (ui-event (list :fade h)))
          (gs-animate :alpha h hw ms :from (gethash hw *gs-alpha*) :to to))))
  0)

(defswift "ui_gravity_drop" (h)
  (let* ((hw (ctl-hwnd (ctl-of h))) (cur (gs-frame hw)))
    (when cur
      (let ((parent (let ((p (c *get-parent* hw))) (if (zerop p) *hwnd* p))))
        (multiple-value-bind (cw ch) (gs-client-size parent)
          (declare (ignore cw))
          (let* ((x (first cur)) (y (second cur))
                 (target (- ch (fourth cur)))
                 (dist (- target y)))
            (when (plusp dist)
              (gs-remove-anims hw :fall)
              ;; the time to fall DIST pixels from rest under the gravity: t = sqrt(2 d / g)
              (let ((ms (round (* 1000.0 (sqrt (/ (* 2.0 dist) (px *gs-gravity*)))))))
                (gs-animate :fall h hw ms :from (list x y) :to target)))))))
    0))

;;; ---- the keyboard: none on a PC, so the views move up for a test, or a real one later ----------------------------

(defvar *gs-kbd-views* '() "The hwnds ui_keyboard_avoid_on recorded.")
(defvar *gs-kbd-origin* (make-hash-table) "hwnd -> its y before it moved up for the keyboard.")

(defswift "ui_keyboard_avoid_on" (h)
  (let ((hw (ctl-hwnd (ctl-of h))))
    (unless (or (zerop hw) (member hw *gs-kbd-views*))
      (push hw *gs-kbd-views*)))
  0)

(defswift "ui_keyboard_post" (height)
  (dolist (hw *gs-kbd-views*)
    (let ((cur (gs-frame hw)))
      (when cur
        (unless (gethash hw *gs-kbd-origin*) (setf (gethash hw *gs-kbd-origin*) (second cur)))
        (let ((orig (gethash hw *gs-kbd-origin*)))
          (gs-place hw (first cur) (if (plusp height) (- orig (px height)) orig))))))
  (when (<= height 0) (clrhash *gs-kbd-origin*))
  0)

;;; ---- input keys: a row of buttons under a text field ----------------------------------------------------------------

(defvar *gs-key-buttons* (make-hash-table) "A key's button hwnd -> (field-handle . the key's text).")
(defvar *gs-key-groups* (make-hash-table) "A field's handle -> its key buttons' hwnds.")

(defun gs-type-into (field text)
  "EM_REPLACESEL: the field's text, with TEXT replacing its selection (undoable), and the field has the focus."
  (let* ((hw (ctl-hwnd (ctl-of field))) (n (length text))
         (buf (modus-int::w32-alloc (* 2 (+ n 1)) t)))
    (wide-at buf text)
    (u *send-message* hw #xC2 1 buf)
    (modus-int::w32-free buf (* 2 (+ n 1)))
    (u *set-focus* hw)))

(defun gs-input-keys (field keys)
  (let* ((fhw (ctl-hwnd (ctl-of field))))
    (unless (zerop fhw)
      (dolist (hw (gethash field *gs-key-groups*))
        (remhash hw *gs-key-buttons*)
        (u *destroy-window* hw))
      (remhash field *gs-key-groups*)
      (let ((frame (gs-frame fhw))
            (parent (let ((p (c *get-parent* fhw))) (if (zerop p) *hwnd* p))))
        (when frame
          (destructuring-bind (x y w h) frame
            (declare (ignore w))
            (let ((bx x) (top (+ y h (px 4))) (made '()))
              (dolist (k keys)
                (let* ((text (if (stringp k) k (princ-to-string k)))
                       (hw (u *create-window* 0 (wide1 "BUTTON") (wide2 text) #x50000000   ; WS_CHILD | WS_VISIBLE
                              bx top (px 44) (px 32) parent 0 *hinst* 0)))
                  (unless (zerop hw)
                    (raise hw)                                                          ; above what was made before
                    (u *send-message* hw #x30 *font* 1)                                 ; WM_SETFONT
                    (setf (gethash hw *gs-key-buttons*) (cons field text))
                    (push hw made)
                    (incf bx (+ (px 44) (px 6))))))
              (setf (gethash field *gs-key-groups*) (nreverse made))
              ;; the row goes when the field does
              (push (lambda ()
                      (dolist (hw (gethash field *gs-key-groups*))
                        (remhash hw *gs-key-buttons*)
                        (u *destroy-window* hw))
                      (remhash field *gs-key-groups*))
                    (gethash field *on-remove*)))))))))

(defun gs-message-hook (msg hwnd wp lp)
  "A key button's click (WM_COMMAND, BN_CLICKED) types its key, and is no tap."
  (declare (ignore hwnd))
  (when (and (= msg #x111) (plusp lp) (zerop (hi16 wp)))
    (let ((entry (gethash lp *gs-key-buttons*)))
      (when entry
        (gs-type-into (car entry) (cdr entry))
        t))))

(pushnew 'gs-message-hook *message-hooks*)

;;; ---- the calls ------------------------------------------------------------------------------------------------------------

(defswift "ui_tap_on" (h)
  (unless (eq (ctl-kind (ctl-of h)) :button)              ; a button's click is WM_COMMAND's :tap already
    (gs-add (make-grec :kind :tap :h h
                       :fire (lambda (arg) (declare (ignore arg)) (ui-event (list :tap h))))))
  0)

(defswift "ui_long_press_on" (h ms)
  (gs-add (make-grec :kind :longpress :h h :button :left :ms (if (plusp ms) ms 500)
                     :fire (lambda (arg) (declare (ignore arg)) (ui-event (list :longpress h)))))
  0)

(defswift "ui_swipe_on" (h dir)
  (gs-add (make-grec :kind :swipe :h h :dir dir
                     :fire (lambda (arg) (declare (ignore arg)) (ui-event (list :swipe h dir)))))
  0)

(defswift "ui_pan_on" (h)
  (gs-add (make-grec :kind :pan :h h
                     :fire (lambda (dx) (ui-event (list :pan h dx)))))
  0)

(defswift "ui_edge_pan_on" (h)
  (gs-add (make-grec :kind :edge :h h
                     :fire (lambda (arg) (declare (ignore arg)) (ui-event (list :edge h)))))
  0)

(defswift "ui_context_menu_on" (h a n)
  (let ((choices (gs-strings (term-arg a n))))
    (flet ((show (arg) (declare (ignore arg)) (gs-pop-menu h :context choices)))
      (gs-add (make-grec :kind :tap :h h :button :right :fire #'show))          ; a right click
      (gs-add (make-grec :kind :longpress :h h :button :left :ms 500 :fire #'show)))) ; a long press
  0)

(defswift "ui_edit_menu_on" (h a n)
  (let ((choices (gs-strings (term-arg a n))))
    (gs-add (make-grec :kind :longpress :h h :button :left :ms 500
                       :fire (lambda (arg) (declare (ignore arg)) (gs-pop-menu h :edit choices)))))
  0)

;; accessibility: the label is remembered for a screen reader; no caption changes
(defvar *gs-labels* (make-hash-table) "A view's handle -> its accessibility label.")

(defswift "ui_set_accessibility" (h a n)
  (setf (gethash h *gs-labels*) (utf8-text a n))
  0)

(defswift "ui_announce" (a n)
  (format t "~&vessel-windows: announce: ~A~%" (utf8-text a n))
  0)

(defswift "ui_input_keys_on" (field a n)
  (gs-input-keys field (term-arg a n))
  0)

(defswift "ui_haptic" (strength) 0)
(defswift "ui_parallax_on" (h amount) 0)

