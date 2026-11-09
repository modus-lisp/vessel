;;;; ios-storybook.lisp -- a storybook for the native components (ios-swift.lisp).  A page with a row of
;;;; story buttons; each story shows one kind of component, wired to its events.  Open it from the Actions
;;;; grid ("storybook").  A story is a function that makes its components and returns their handles; the
;;;; storybook removes them when another story replaces it.
;;;;
;;;; The stories are the test surface too: kiln/test/ios-sim-ui.sh taps and reads them.

(defpackage #:kiln-storybook
  (:use #:cl)
  (:export #:storybook #:close-storybook))

(in-package #:kiln-storybook)

(defparameter *background* #x16181c)

(defvar *shelf* nil "The handles of the storybook's own views: background, title, story buttons, close.")
(defvar *story* nil "The handles of the story on show.")
(defvar *story-name* nil)
(defvar *taps* 0)

(defun %forget (handles)
  (dolist (h handles) (kiln-swift:ui-remove h)))

;;; ---- the stories --------------------------------------------------------------------------------

(defun %story-label ()
  (list (kiln-swift:ui-label "A plain label." 24 200 350 30)
        (kiln-swift:ui-label "A long label: it wraps across as many lines as its words need, so the frame's height is only a first guess."
                             24 240 350 90)))

(defun %story-button ()
  (let* ((count (kiln-swift:ui-label "tapped 0 times" 24 200 350 30))
         (button (kiln-swift:ui-button "tap me" 24 240 160 48)))
    (setf *taps* 0)
    (kiln-swift:on-tap button
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-set-text count (format nil "tapped ~D times" (incf *taps*)))))
    (list count button)))

(defun %story-switch ()
  (let* ((state (kiln-swift:ui-label "off" 24 200 350 30))
         (switch (kiln-swift:ui-switch nil 24 240)))
    (kiln-swift:on-event :switch switch
                         (lambda (value h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text state (if (= value 1) "on" "off"))))
    (list state switch)))

(defun %story-slider ()
  (let* ((readout (kiln-swift:ui-label "value 50" 24 200 350 30))
         (slider (kiln-swift:ui-slider 50 24 240 350 30)))
    (kiln-swift:on-event :slider slider
                         (lambda (value h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "value ~D" value))))
    (list readout slider)))

(defun %story-text ()
  (let* ((field (kiln-swift:ui-text-field "type here" 24 200 350 36))
         (mirror (kiln-swift:ui-label "" 24 250 350 30)))
    (kiln-swift:on-event :text field
                         (lambda (value h)
                           (declare (ignore value))
                           (kiln-swift:ui-set-text mirror (kiln-swift:ui-text-value h))))
    (list field mirror)))

(defparameter *stories*
  (list (cons "label" #'%story-label)
        (cons "button" #'%story-button)
        (cons "switch" #'%story-switch)
        (cons "slider" #'%story-slider)
        (cons "text" #'%story-text)))

;;; ---- the storybook ------------------------------------------------------------------------------

(defun %show (name)
  "Show the story NAME in place of the one on show."
  (%forget *story*)
  (setf *story* nil)
  (setf *story-name* name)
  (setf *story* (funcall (cdr (assoc name *stories* :test #'string=)))))

(defun storybook ()
  "Open the storybook (closing any open one first)."
  (close-storybook)
  (let ((background (kiln-swift:ui-panel 0 0 0 0 *background*))
        (title (kiln-swift:ui-label "storybook" 24 60 250 30))
        (x 24))
    (setf *shelf* (list background title))
    (dolist (story *stories*)
      (let* ((name (car story))
             (button (kiln-swift:ui-button name x 120 68 36)))
        (push button *shelf*)
        (kiln-swift:on-tap button (lambda (value h) (declare (ignore value h)) (%show name)))
        (incf x 76)))
    ;; top right: the keyboard covers the bottom of the screen while a text field has focus
    (let ((closer (kiln-swift:ui-button "close" 290 60 90 36)))
      (push closer *shelf*)
      (kiln-swift:on-tap closer (lambda (value h) (declare (ignore value h)) (close-storybook))))
    (%show "label")
    :storybook))

(defun close-storybook ()
  "Close the storybook: its views leave the screen, and the desk shows again."
  (%forget *story*)
  (setf *story* nil)
  (%forget *shelf*)
  (setf *shelf* nil)
  (setf *story-name* nil)
  :closed)
