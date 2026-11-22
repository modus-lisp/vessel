;;;; notify.lisp -- the system's notifications from Lisp, on the Mac and the phone: say what they are for, ask, send
;;;; one (now or later), hear it clicked.  The Swift side is modus's host/ios/swift/Notify.swift (notify_*).
;;;;
;;;;   (notify-permission)                       :not-asked :allowed :refused
;;;;   (notify-ask fn)                           the system's question (once); FN gets T or NIL as answered
;;;;   (notify "Title" "Body" &key after on-click)   a notification, now or AFTER seconds; ON-CLICK, of no
;;;;                                             arguments, is called if it is clicked
;;;;   (notify-badge 3)                          the app icon's number (0 clears it)
;;;;
;;;; Ask only when the person has asked for something that needs it, and say first what for (an alert of the app's
;;;; own, as hello3d's head-following does): the system asks once, and a refusal is for good.

(defpackage #:kiln-notify
  (:use #:cl)
  (:export #:notify-permission #:notify-ask #:notify #:notify-badge))

(in-package #:kiln-notify)

(defun %call (name &rest args) (apply #'kiln-swift:swift-call name args))

(defun notify-permission ()
  (case (%call "notify_permission") (0 :not-asked) (1 :allowed) (t :refused)))

(defun notify-ask (fn)
  "Ask the system for notifications; FN gets T if allowed, NIL if not."
  (kiln-swift:on-event :notifyask 0 (lambda (ok h) (declare (ignore h)) (funcall fn (eql ok 1))))
  (%call "notify_ask"))

(defun notify (title body &key (after 0) on-click)
  "A notification with TITLE and BODY, now or AFTER seconds; ON-CLICK is called if it is clicked.  Its handle."
  (let* ((tb (kiln-swift::%octets title)) (bb (kiln-swift::%octets body))
         (h (%call "notify_post" (kiln-swift::%addr tb) (length tb) (kiln-swift::%addr bb) (length bb)
                   (max 0 (round after)))))
    (when on-click
      (kiln-swift:on-event :notified h (lambda (v h) (declare (ignore v h)) (funcall on-click))))
    h))

(defun notify-badge (count) (%call "notify_badge" (max 0 count)))
