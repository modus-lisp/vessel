;;;; motion.lisp -- how the device itself is held, from Lisp: its turn against how it was held when following began
;;;; (or was last recentered), as a rotation.  The headphones' turn is SOUND.LISP's SPATIAL-HEAD-ATTITUDE; this is
;;;; the phone's, beside it (modus's host/ios/swift/Motion.swift).  A Mac has none: DEVICE-MOTION is NIL there.
;;;;
;;;;   (device-motion on)                   follow the device (ON) or stop; T if it can be followed here
;;;;   (device-attitude)                    a 3x3 rotation, row-major (a vector of nine), from the device's own axes
;;;;                                        (x to the right of its screen held upright, y to its top, z out of it)
;;;;                                        to the screen's at the neutral pose (x right, y up, z out, as the GPU's,
;;;;                                        in the interface's orientation); NIL while nothing is reported
;;;;   (device-recenter)                    how it is held now is neutral

(defpackage #:kiln-motion
  (:use #:cl)
  (:export #:device-motion #:device-attitude #:device-recenter))

(in-package #:kiln-motion)

(defun %call (name &rest args)
  (and (find-package "KILN-SWIFT") (apply #'kiln-swift:swift-call name args)))

(defun device-motion (on)
  "Follow the device's motion (ON), or stop.  T if it can be followed here (no permission is asked)."
  (eql 1 (%call "device_motion" (if on 1 0))))

(defvar *rotation* (make-array 9 :element-type 'single-float :initial-element 0.0))

(defun device-attitude ()
  "The device's turn since its neutral pose, a 3x3 rotation (nine floats, row-major) from its own axes to the
   screen's; NIL while nothing is reported.  The vector is reused: read it before asking again."
  (when (eql 1 (%call "device_attitude" (kiln-swift::%addr *rotation*)))
    *rotation*))

(defun device-recenter ()
  "How the device is held now is neutral."
  (%call "device_recenter"))
