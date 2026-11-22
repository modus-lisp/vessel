;;;; microphone.lisp -- the microphone's own sound, from Lisp: mono signed 16-bit samples, the speaker's format, at
;;;; the rate asked for.  The Swift side is modus's host/ios/swift/Microphone.swift (mic_*), the same on the phone and
;;;; on the Mac, called through ios-swift.lisp's SWIFT-CALL.  Polled: read what has come, once a frame or so; four
;;;; seconds are kept waiting, and after that the oldest give way (MIC-DROPPED counts them).
;;;;
;;;;   (mic-start &optional rate)   ask for the microphone (the first time), then capture at RATE Hz (48000)
;;;;   (mic-state)                  :off :asking :capturing :denied :failed
;;;;   (mic-read &optional buffer)  the samples waiting, into BUFFER (a (signed-byte 16) vector; one kept and reused
;;;;                                if none): (values buffer count)
;;;;   (mic-waiting)                how many samples are waiting
;;;;   (mic-level)                  the input level now, 0.0 to 1.0 (RMS)
;;;;   (mic-dropped)                samples lost since the start, read too slowly
;;;;   (mic-stop)

(defpackage #:kiln-mic
  (:use #:cl)
  (:export #:mic-start #:mic-state #:mic-read #:mic-waiting #:mic-level #:mic-dropped #:mic-stop))

(in-package #:kiln-mic)

(defun mic-start (&optional (rate 48000))
  "Ask for the microphone and capture from it at RATE Hz.  MIC-STATE says when it has started (or may not)."
  (kiln-swift:swift-call "mic_start" rate)
  t)

(defun mic-state ()
  (case (kiln-swift:swift-call "mic_state")
    (0 :off) (1 :asking) (2 :capturing) (3 :denied) (t :failed)))

(defvar *samples* nil "The buffer MIC-READ reads into when given none: a second at 48 kHz, kept and reused.")

(defun mic-read (&optional buffer)
  "The samples waiting, oldest first, into BUFFER (default: one kept and reused -- copy what is to be kept), as many
   as fit: (values buffer count)."
  (let ((buf (or buffer
                 *samples*
                 (setf *samples* (make-array 48000 :element-type '(signed-byte 16) :initial-element 0)))))
    (values buf (kiln-swift:swift-call "mic_read" (kiln-swift::%addr buf) (length buf)))))

(defun mic-waiting () (kiln-swift:swift-call "mic_waiting"))

(defun mic-dropped () (kiln-swift:swift-call "mic_dropped"))

(defun mic-level ()
  "The input level now, as RMS of full scale: 0.0 silence, 1.0 the loudest."
  (/ (kiln-swift:swift-call "mic_level") 1000000.0))

(defun mic-stop () (kiln-swift:swift-call "mic_stop") t)
