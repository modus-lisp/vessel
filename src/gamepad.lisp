;;;; gamepad.lisp -- game controllers from Lisp: an Xbox, PlayStation or Switch Pro controller, an MFi pad, the Apple
;;;; TV remote.  The Swift side is modus's host/ios/swift/Gamepad.swift (pad_*), the same on the Mac, the phone and
;;;; the TV.  POLLED, as a game reads its input: once a frame, PAD-READ, then ask the state what it says.
;;;;
;;;;   (pad-count)                     controllers connected
;;;;   (pad-name index)                "Xbox Wireless Controller" ...
;;;;   (pad-read index &optional state)   its state now (a vector, reused when STATE is given), or NIL
;;;;   (pad-axis state :left-x)        -1..1 (up and right positive) past a small dead zone: :left-x :left-y
;;;;                                   :right-x :right-y :dpad-x :dpad-y; and 0..1 for :left-trigger :right-trigger
;;;;   (pad-button state :a)           true while held: :a :b :x :y (south east west north), :left-shoulder
;;;;                                   :right-shoulder :left-stick :right-stick :menu :options :home
;;;;   (pad-light index #xRRGGBB)      a PlayStation controller's light bar
;;;;   (pad-rumble index strength ms)  a buzz, STRENGTH 0.0..1.0, where the controller can

(defpackage #:kiln-pad
  (:use #:cl)
  (:export #:pad-count #:pad-name #:pad-read #:pad-axis #:pad-button #:pad-light #:pad-rumble #:*dead-zone*))

(in-package #:kiln-pad)

(defconstant +floats+ 19 "The state's length: Gamepad.swift's PAD_FLOATS.")

(defparameter *dead-zone* 0.12 "How far a stick moves before it counts: a resting stick is never quite still.")

(defparameter *axes* '(:left-x 0 :left-y 1 :right-x 2 :right-y 3 :left-trigger 4 :right-trigger 5 :dpad-x 6 :dpad-y 7))
(defparameter *buttons* '(:a 8 :b 9 :x 10 :y 11 :left-shoulder 12 :right-shoulder 13 :left-stick 14 :right-stick 15
                          :menu 16 :options 17 :home 18))

(defun pad-count () (kiln-swift:swift-call "pad_count"))

(defun pad-name (index)
  (let* ((buf (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0))
         (n (kiln-swift:swift-call "pad_name" index (kiln-swift::%addr buf) 256)))
    (if (< 0 n 257) (seal:utf8-decode buf :end n) "")))

(defun pad-read (index &optional state)
  "Controller INDEX's state now, into STATE (a single-float vector of at least 19) or a new one; NIL when there
   is no such controller."
  (let ((s (or state (make-array +floats+ :element-type 'single-float :initial-element 0.0))))
    (and (plusp (kiln-swift:swift-call "pad_state" index (kiln-swift::%addr s) (min (length s) +floats+)))
         s)))

(defun pad-axis (state axis)
  "AXIS of STATE: a stick or the d-pad -1..1, a trigger 0..1, zero within *DEAD-ZONE* of rest."
  (let ((v (aref state (getf *axes* axis))))
    (if (< (abs v) *dead-zone*) 0.0 v)))

(defun pad-button (state button)
  "Is BUTTON held in STATE?"
  (> (aref state (getf *buttons* button)) 0.5))

(defun pad-light (index rgb) (kiln-swift:swift-call "pad_light" index rgb))

(defun pad-rumble (index strength ms)
  (kiln-swift:swift-call "pad_rumble" index (round (* 1000 (max 0.0 (min 1.0 strength)))) (round ms)))
