;;;; hello3d-watch.lisp -- Hello 3D's watch app: the watch's sensors, sent to the phone as it plays (link.lisp).
;;;; Its turn (motion.lisp: the same calls as the phone's own), the heart rate (through a workout, which also keeps
;;;; it running with the wrist down), the Digital Crown and a tap.  On the watch, in a slim world of its own (kiln's
;;;; boot/hello3d-watch-world.lisp); the phone's hello3d.lisp hears it.
;;;;
;;;; What goes to the phone, each a term:
;;;;   (:m r0 .. r8 bpm)   about fifteen times a second: the watch's rotation (motion.lisp's, row-major) in
;;;;                       ten-thousandths, and the heart rate in beats a minute (0 until there is one)
;;;;   (:crown value)      the crown turned: 0 to 100
;;;;   (:tap)              the Flip button
;;;; And from the phone: (:recenter) -- how the watch is worn now is neutral.
;;;;
;;;; The app's modus.args say  --core @kiln.core --eval (kiln-hello3d-watch:run)

(defpackage #:kiln-hello3d-watch
  (:use #:cl)
  (:export #:run))

(in-package #:kiln-hello3d-watch)

(defvar *sx* 1 "This screen's width over the design's 208 points.")
(defvar *sy* 1 "This screen's height over the design's 248 points.")
(defun %x (n) (round (* n *sx*)))
(defun %y (n) (round (* n *sy*)))

(defvar *status* nil "The label saying whether the phone is there.")
(defvar *heart* nil "The label with the heart rate.")
(defvar *frame* 0)
(defvar *bpm-shown* -1)

(defun %heart-rate () (kiln-swift:swift-call "heart_rate"))
(defun %heart-state () (kiln-swift:swift-call "heart_state"))

(defun show-state (state)
  (kiln-swift:ui-set-text *status* (case state
                                     (:reachable "phone: playing")
                                     (:linked "phone: open Hello 3D")
                                     (t "phone: not linked"))))

(defun send-motion ()
  "The watch's rotation and the heart rate, to the phone (when it is listening)."
  (let ((r (kiln-motion:device-attitude)))
    (when r
      (kiln-link:link-send
       (list* :m (append (loop for i below 9 collect (round (* 10000 (aref r i))))
                         (list (%heart-rate))))))))

(defun show-heart ()
  (let ((bpm (%heart-rate)) (state (%heart-state)))
    (unless (eql bpm *bpm-shown*)
      (setf *bpm-shown* bpm)
      (kiln-swift:ui-set-text *heart* (cond ((plusp bpm) (format nil "♥ ~D" bpm))
                                            ((eql state 3) "♥ off")
                                            (t "♥ …"))))))

(defun tick ()
  "Each frame of the watch's loop (30 a second): the motion every other one; the heart rate's label twice a second."
  (incf *frame*)
  (when (evenp *frame*) (send-motion))
  (when (zerop (mod *frame* 15)) (show-heart)))

(defun build ()
  (let ((w (kiln-swift:ui-screen-width)) (h (kiln-swift:ui-screen-height)))
    (when (and w (plusp w)) (setf *sx* (/ w 208)))
    (when (and h (plusp h)) (setf *sy* (/ h 248))))
  (kiln-swift:ui-label "Hello 3D" (%x 8) (%y 4) (%x 192) (%y 24))
  (setf *status* (kiln-swift:ui-label "phone: …" (%x 8) (%y 28) (%x 192) (%y 20))
        *heart* (kiln-swift:ui-label "♥ …" (%x 8) (%y 50) (%x 192) (%y 28)))
  (let ((crown (kiln-swift:ui-crown "speed" (%x 8) (%y 82) (%x 192) (%y 90)))
        (flip (kiln-swift:ui-button "Flip" (%x 8) (%y 180) (%x 192) (%y 44))))
    (kiln-swift:ui-set-value crown 50)
    (kiln-swift:on-event :crown crown (lambda (v h) (declare (ignore h)) (kiln-link:link-send (list :crown (round v)))))
    (kiln-swift:on-tap flip (lambda (v h) (declare (ignore v h)) (kiln-link:link-send (list :tap))))))

(defun run ()
  "The watch app: its screen, the link and the sensors started, then a frame at a time for good."
  (build)
  (kiln-link:link-open)
  (kiln-link:on-link-state #'show-state)
  ;; the phone's: recenter (its head following just began, so all three start from here)
  (kiln-link:on-link (lambda (term) (when (and (consp term) (eq (first term) :recenter)) (kiln-motion:device-recenter))))
  (show-state (kiln-link:link-state))
  (kiln-motion:device-motion t)
  (kiln-swift:swift-call "heart_start")
  (pushnew 'tick cl-user::*kiln-frame-hooks*)
  (pushnew 'kiln-swift:swift-pump cl-user::*kiln-frame-hooks*)
  (loop
    (dolist (f cl-user::*kiln-frame-hooks*)
      (handler-case (funcall f)
        (error (e) (format t "~&hello3d-watch: ~A: ~A~%" f e))))
    (sleep cl-user::*kiln-frame-seconds*)))
