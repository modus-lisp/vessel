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
                           ;; the slider sends a double, as a term
                           (kiln-swift:ui-set-text readout (format nil "value ~D" (round value)))))
    (list readout slider)))

(defun %story-text ()
  (let* ((field (kiln-swift:ui-text-field "type here" 24 200 350 36))
         (mirror (kiln-swift:ui-label "" 24 250 350 30))
         (done (kiln-swift:ui-button "done" 24 300 120 40)))
    (kiln-swift:on-event :text field
                         (lambda (value h)
                           (declare (ignore value))
                           (kiln-swift:ui-set-text mirror (kiln-swift:ui-text-value h))))
    ;; the keyboard covers the bottom of the screen; "done" gives it up
    (kiln-swift:on-tap done (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-resign)))
    (list field mirror done)))

(defun %story-stack ()
  (let* ((stack (kiln-swift:ui-stack t 8 24 200 350 180))
         (top (kiln-swift:ui-label "first" 0 0 300 24))
         (middle (kiln-swift:ui-label "second" 0 0 300 24))
         (bottom (kiln-swift:ui-label "third" 0 0 300 24)))
    (dolist (child (list top middle bottom))
      (kiln-swift:ui-add stack child))
    (list stack top middle bottom)))

(defun %story-scroll ()
  "Thirty items in a scroll view: the last is below the fold until the view is dragged."
  (let* ((scroll (kiln-swift:ui-scroll 24 200 350 560))
         (items (loop for i from 1 to 30
                      collect (kiln-swift:ui-label (format nil "item ~D" i) 0 0 300 24))))
    (dolist (item items)
      (kiln-swift:ui-add scroll item))
    (cons scroll items)))

(defun %story-image ()
  (let ((picture (kiln-swift:ui-image "star.fill" 24 200 120 120))
        (name (kiln-swift:ui-label "star.fill" 24 330 350 30)))
    (list picture name)))

(defun %story-list ()
  "Twenty rows: only the visible ones are made, and a tap on one shows which it was."
  (let* ((table (kiln-swift:ui-list 24 200 350 560))
         (selected (kiln-swift:ui-label "selected: nothing" 24 780 350 30)))
    (kiln-swift:ui-list-set-data table
                                 (loop for i from 1 to 20
                                       collect (list :id i :title (format nil "row ~D" i)
                                                     :subtitle "a record, sent as a term")))
    (kiln-swift:on-event :select table
                         (lambda (row h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text selected (format nil "selected: row ~D" (1+ row)))))
    (list table selected)))

(defparameter *stories*
  (list (cons "label" #'%story-label)
        (cons "button" #'%story-button)
        (cons "switch" #'%story-switch)
        (cons "slider" #'%story-slider)
        (cons "text" #'%story-text)
        (cons "stack" #'%story-stack)
        (cons "scroll" #'%story-scroll)
        (cons "image" #'%story-image)
        (cons "list" #'%story-list)))

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
        (title (kiln-swift:ui-label "storybook" 24 60 250 30)))
    (setf *shelf* (list background title))
    ;; five to a row; the stories start below the second row
    (loop for story in *stories*
          for i from 0
          do (let* ((name (car story))
                    (button (kiln-swift:ui-button name (+ 24 (* 76 (mod i 5))) (+ 120 (* 44 (floor i 5)))
                                                  68 36)))
               (push button *shelf*)
               (kiln-swift:on-tap button (lambda (value h) (declare (ignore value h)) (%show name)))))
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
