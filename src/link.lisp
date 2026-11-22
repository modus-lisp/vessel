;;;; link.lisp -- a phone app and its watch app talking, from Lisp: a term sent from one arrives at the other as an
;;;; event (modus's host/ios/swift/Link.swift, WatchConnectivity).  Sent only while both apps are running and the
;;;; watch is near: a sensor's readings, not a store of record.  The same on the phone and the watch.
;;;;
;;;;   (link-open)                  start the link: T if this device can have one
;;;;   (link-send term)             send TERM (fixnums, strings, symbols, conses, doubles -- not single-floats);
;;;;                                T if it went, NIL while the other side is not reachable
;;;;   (link-state)                 :reachable, :linked (the other app is there, not running now), or NIL
;;;;   (on-link fn)                 FN called with each term that arrives
;;;;   (on-link-state fn)           FN called with the new state when it changes

(defpackage #:kiln-link
  (:use #:cl)
  (:export #:link-open #:link-send #:link-state #:on-link #:on-link-state))

(in-package #:kiln-link)

(defvar *link* nil "The link's handle, once open: its events come from it.")

(defun %call (name &rest args)
  (and (find-package "KILN-SWIFT") (apply #'kiln-swift:swift-call name args)))

(defun link-open ()
  "Start the link.  T if there can be one here (a phone or a watch; not a Mac)."
  (unless *link*
    (let ((h (%call "link_open")))
      (when (and h (plusp h)) (setf *link* h))))
  (and *link* t))

(defun link-send (term)
  "Send TERM to the other side: T if it went, NIL if it is not reachable now."
  (when *link*
    (let ((b (kiln-swift::%term-octets term)))
      (eql 1 (%call "link_send" (kiln-swift::%addr b) (length b))))))

(defun %state (n) (case n (2 :reachable) (1 :linked) (t nil)))

(defun link-state ()
  "How the link is now: :REACHABLE (both apps running), :LINKED (the other is installed), or NIL."
  (%state (%call "link_state")))

(defun on-link (fn)
  "Call FN with each term the other side sends."
  (when *link*
    (kiln-swift:on-event :link *link* (lambda (term h) (declare (ignore h)) (funcall fn term)))))

(defun on-link-state (fn)
  "Call FN with the link's state (as LINK-STATE says it) each time it changes."
  (when *link*
    (kiln-swift:on-event :linkstate *link* (lambda (n h) (declare (ignore h)) (funcall fn (%state n))))))
