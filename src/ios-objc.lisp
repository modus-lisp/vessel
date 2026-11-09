;;;; ios-objc.lisp -- the Objective-C runtime from a modus image: classes, selectors and messages,
;;;; so a modus app can drive UIKit, and any Swift class that inherits NSObject or is @objc, by
;;;; its runtime name.  The C side is host/ios/modus-objc.m (pseudo-syscall 1040), and the frame
;;;; layout is written down there.  Everything here is a value: an object is its address, a SEL is
;;;; its address, NIL is 0, and a number is itself.
;;;;
;;;;   (objc-class "UIApplication")                  the class object
;;;;   (objc-sel "setText:")                         the selector
;;;;   (objc-send recv "setText:" arg)               a message: the returned word
;;;;   (objc-string "hello")                         an NSString
;;;;   (objc-alert "Title" "Message")                a native alert over the app's window
;;;;
;;;; The arguments of a message are at most four, and integers or pointers.

(defpackage #:kiln-objc
  (:use #:cl)
  (:export #:objc-class #:objc-sel #:objc-send #:objc-string #:objc-alert))

(in-package #:kiln-objc)

(defparameter *frame-bytes* 384)
(defparameter *name-at* 128)

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun %fresh-frame ()
  (make-array *frame-bytes* :element-type '(unsigned-byte 8) :initial-element 0))

(defun %put-word (f off w)
  (dotimes (i 8) (setf (aref f (+ off i)) (ldb (byte 8 (* 8 i)) w))))

(defun %get-word (f off)
  (let ((w 0))
    (dotimes (i 8 w)
      (setf w (+ w (ash (aref f (+ off i)) (* 8 i)))))))

(defun %put-name (f name)
  "NAME, NUL-terminated, at the frame's name field."
  (when (>= (length name) (- *frame-bytes* *name-at* 1))
    (error "objc: the name ~S is too long for a frame" name))
  (loop for c across name for i from *name-at*
        do (setf (aref f i) (logand (char-code c) #xff)))
  (setf (aref f (+ *name-at* (length name))) 0))

(defun %call (f)
  "Run the frame F through the runtime; its result word."
  (let ((rc (cl-user::%kiln-sys 1040 (%addr f) 0 0)))
    (when (< rc 0) (error "objc: the runtime refused the call (errno ~D)" (- rc)))
    (%get-word f 64)))

(defun objc-class (name)
  "The class object NAME (a Swift class is 'Module.Class'); 0 if there is no such class."
  (let ((f (%fresh-frame)))
    (%put-name f name)
    (%put-word f 0 1)
    (%call f)))

(defun objc-sel (name)
  "The selector NAME, e.g. \"setText:\"."
  (let ((f (%fresh-frame)))
    (%put-name f name)
    (%put-word f 0 2)
    (%call f)))

(defun objc-send (recv sel &rest args)
  "Send SEL (a selector name, or a SEL word) to RECV with ARGS (at most four words); the result."
  (when (> (length args) 4) (error "objc-send: at most four arguments, not ~D" (length args)))
  (let ((f (%fresh-frame)))
    (%put-word f 0 3)
    (%put-word f 8 (or recv 0))
    (%put-word f 16 (if (stringp sel) (objc-sel sel) sel))
    (%put-word f 24 (length args))
    (loop for a in args for i from 0 do (%put-word f (+ 32 (* 8 i)) (or a 0)))
    (%call f)))

(defun objc-string (text)
  "An NSString of TEXT (ASCII; a longer text than a frame holds is an error)."
  (let ((f (%fresh-frame)))
    (%put-name f text)
    (%put-word f 0 3)
    (%put-word f 8 (objc-class "NSString"))
    (%put-word f 16 (objc-sel "stringWithUTF8String:"))
    (%put-word f 24 1)
    (%put-word f 32 (+ (%addr f) *name-at*))
    (%call f)))

(defun objc-alert (title message)
  "A native UIAlertController with one OK button, shown over the app's window.  The first
   use of UIKit from a modus app: UIApplication's key window, then presentViewController."
  (let* ((app (objc-send (objc-class "UIApplication") "sharedApplication"))
         (window (objc-send app "keyWindow"))
         (root (objc-send window "rootViewController"))
         (alert (objc-send (objc-class "UIAlertController")
                           "alertControllerWithTitle:message:preferredStyle:"
                           (objc-string title) (objc-string message) 1))
         (ok (objc-send (objc-class "UIAlertAction") "actionWithTitle:style:handler:"
                        (objc-string "OK") 0 0)))
    (objc-send alert "addAction:" ok)
    (objc-send root "presentViewController:animated:completion:" alert 1 0)
    alert))
