;;;; watch-storybook.lisp -- the storybook on the watch: the native components (ios-swift.lisp's, which
;;;; modus's host/watch/modus-watch-ui.swift makes in SwiftUI), one story at a time on a 208 x 248 screen.
;;;; The story's name is the title; "prev" and "next" step through them.  kiln/test/watch-sim-ui.sh taps
;;;; and reads them.
;;;;
;;;; The same calls as the phone's storybook, and two only the watch has: a gauge and the Digital Crown.

(defpackage #:kiln-watch-storybook
  (:use #:cl)
  (:export #:watch-storybook #:close-watch-storybook))

(in-package #:kiln-watch-storybook)

(defparameter *background* #x000000)
(defparameter *x* 8)
(defparameter *w* 192)
(defparameter *top* 54 "The top of a story's area, below the title.")

(defvar *shelf* nil "The storybook's own views: background, title, prev, next.")
(defvar *title* nil)
(defvar *story* nil "The handles of the story on show.")
(defvar *index* 0)
(defvar *taps* 0)

(defun %forget (handles)
  (dolist (h handles) (kiln-swift:ui-remove h)))

(defun %readout (text &optional (y *top*))
  (kiln-swift:ui-label text *x* y *w* 22))

(defmacro %on (kind handle (value) &body body)
  `(kiln-swift:on-event ,kind ,handle (lambda (,value h) (declare (ignorable ,value h)) ,@body)))

;;; ---- the stories --------------------------------------------------------------------------------

(defun %story-label ()
  (list (%readout "A plain label.")
        (kiln-swift:ui-label "A long label wraps across the lines its words need." *x* 80 *w* 80)))

(defun %story-button ()
  (let ((count (%readout "tapped 0 times"))
        (button (kiln-swift:ui-button "tap me" *x* 90 *w* 40)))
    (setf *taps* 0)
    (%on :tap button (v) (kiln-swift:ui-set-text count (format nil "tapped ~D times" (incf *taps*))))
    (list count button)))

(defun %story-switch ()
  (let ((state (%readout "off"))
        (switch (kiln-swift:ui-switch nil *x* 90)))
    (kiln-swift:ui-set-frame switch *x* 90 *w* 40)
    (%on :switch switch (v) (kiln-swift:ui-set-text state (if (eql v 1) "on" "off")))
    (list state switch)))

(defun %story-slider ()
  (let ((readout (%readout "value 50"))
        (slider (kiln-swift:ui-slider 50 *x* 90 *w* 40)))
    (%on :slider slider (v) (kiln-swift:ui-set-text readout (format nil "value ~D" (round v))))
    (list readout slider)))

(defun %story-stepper ()
  (let ((readout (%readout "step 5"))
        (stepper (kiln-swift:ui-stepper 5 *x* 90)))
    (kiln-swift:ui-set-frame stepper *x* 90 *w* 60)
    (%on :step stepper (v) (kiln-swift:ui-set-text readout (format nil "step ~D" (round v))))
    (list readout stepper)))

(defun %story-progress ()
  (let* ((readout (%readout "progress 30"))
         (bar (kiln-swift:ui-progress 30 *x* 90 *w* 20))
         (more (kiln-swift:ui-button "more" *x* 120 *w* 40))
         (n 30))
    (%on :tap more (v)
      (setf n (min 100 (+ n 10)))
      (kiln-swift:ui-set-value bar n)
      (kiln-swift:ui-set-text readout (format nil "progress ~D" n)))
    (list readout bar more)))

(defun %story-gauge ()
  (let* ((readout (%readout "gauge 40"))
         (gauge (kiln-swift:ui-gauge 40 "fuel" 74 84 60 60))
         (more (kiln-swift:ui-button "fill" *x* 150 *w* 40))
         (n 40))
    (%on :tap more (v)
      (setf n (min 100 (+ n 20)))
      (kiln-swift:ui-set-value gauge n)
      (kiln-swift:ui-set-text readout (format nil "gauge ~D" n)))
    (list readout gauge more)))

(defun %story-picker ()
  ;; the wheel turns with the crown or a drag, which a test cannot give it: "blue" turns it from code, as the
  ;; phone's picker story does
  (let ((readout (%readout "picked: none"))
        (picker (kiln-swift:ui-picker '("red" "green" "blue") *x* 78 *w* 64))
        (blue (kiln-swift:ui-button "pick blue" *x* 146 *w* 40)))
    (%on :select picker (v)
      (kiln-swift:ui-set-text readout (format nil "picked: ~A" (nth v '("red" "green" "blue")))))
    (%on :tap blue (v)
      (kiln-swift:ui-set-value picker 2)
      (kiln-swift:ui-set-text readout "picked: blue"))
    (list readout picker blue)))

(defun %story-list ()
  (let ((readout (%readout "selected: none"))
        (list (kiln-swift:ui-list *x* 78 *w* 122)))
    (kiln-swift:ui-list-set-data list (loop for i from 1 to 12 collect (list :title (format nil "row ~D" i))))
    (%on :select list (v) (kiln-swift:ui-set-text readout (format nil "selected: row ~D" (1+ v))))
    (list readout list)))

(defun %story-crown ()
  (let ((readout (%readout "crown 0"))
        (crown (kiln-swift:ui-crown "turn" *x* 80 *w* 100)))
    (%on :crown crown (v) (kiln-swift:ui-set-text readout (format nil "crown ~D" (round v))))
    (list readout crown)))

(defun %story-alert ()
  (let ((readout (%readout "answer: none"))
        (button (kiln-swift:ui-button "ask" *x* 90 *w* 40)))
    (%on :tap button (v)
      (let ((alert (kiln-swift:ui-alert "Really?" "A question with two answers.")))
        (kiln-swift:ui-alert-action alert "yes")
        (kiln-swift:ui-alert-action alert "no")
        (%on :alert alert (i) (kiln-swift:ui-set-text readout (format nil "answer: ~A" (if (eql i 0) "yes" "no"))))
        (kiln-swift:ui-alert-present alert)))
    (list readout button)))

(defun %story-sheet ()
  (let ((readout (%readout "sheet: closed"))
        (button (kiln-swift:ui-button "open" *x* 90 *w* 40)))
    (%on :tap button (v)
      (let* ((modal (kiln-swift:ui-modal))
             (inside (kiln-swift:ui-label "in the sheet" 8 20 180 22))
             (shut (kiln-swift:ui-button "shut" 8 60 180 40)))
        (kiln-swift:ui-add modal inside)
        (kiln-swift:ui-add modal shut)
        (%on :tap shut (v)
          (kiln-swift:ui-modal-dismiss modal)
          (kiln-swift:ui-set-text readout "sheet: shut"))
        (kiln-swift:ui-modal-present modal)))
    (list readout button)))

(defun %story-nav ()
  (let ((readout (%readout "nav: not open"))
        (button (kiln-swift:ui-button "navigate" *x* 90 *w* 40)))
    (%on :tap button (v)
      (let* ((nav (kiln-swift:ui-nav-controller))
             (root (kiln-swift:ui-nav-root nav))
             (go (kiln-swift:ui-button "go deeper" 8 20 180 40)))
        (kiln-swift:ui-add root go)
        (%on :tap go (v)
          (let ((page (kiln-swift:ui-nav-push-view nav "deeper")))
            (kiln-swift:ui-add page (kiln-swift:ui-label "the pushed page" 8 20 180 22))))
        (%on :dismiss nav (v) (kiln-swift:ui-set-text readout "nav: closed"))
        (kiln-swift:ui-modal-present nav)))
    (list readout button)))

(defun %story-tabs ()
  (let ((readout (%readout "tab 0"))
        (button (kiln-swift:ui-button "open tabs" *x* 90 *w* 40)))
    (%on :tap button (v)
      (let ((tabs (kiln-swift:ui-tab-controller '("one" "two"))))
        (kiln-swift:ui-add (kiln-swift:ui-tab-content tabs 0) (kiln-swift:ui-label "first tab" 8 20 180 22))
        (kiln-swift:ui-add (kiln-swift:ui-tab-content tabs 1) (kiln-swift:ui-label "second tab" 8 20 180 22))
        (%on :select tabs (i) (kiln-swift:ui-set-text readout (format nil "tab ~D" i)))
        (kiln-swift:ui-modal-present tabs)))
    (list readout button)))

(defun %story-date ()
  (let ((readout (%readout "date: unchanged"))
        (picker (kiln-swift:ui-date-picker 1767225600 *x* 90)))    ; 2026-01-01 UTC (Dec 31 west of it)
    (kiln-swift:ui-set-frame picker *x* 90 *w* 60)
    (%on :date picker (v) (kiln-swift:ui-set-text readout "date: changed"))
    (list readout picker)))

(defun %story-image ()
  (list (%readout "a symbol")
        (kiln-swift:ui-image "star.fill" 74 90 60 60)))

(defun %story-text ()
  (let ((mirror (%readout ""))
        (field (kiln-swift:ui-text-field "say something" *x* 90 *w* 40)))
    (%on :text field (v) (kiln-swift:ui-set-text mirror (kiln-swift:ui-text-value h)))
    (list mirror field)))

(defun %story-stack ()
  (let ((stack (kiln-swift:ui-stack nil 8 *x* 90 *w* 40))
        (row (list (kiln-swift:ui-image "sun.max.fill" 0 0 30 30)
                   (kiln-swift:ui-image "moon.fill" 0 0 30 30)
                   (kiln-swift:ui-image "cloud.fill" 0 0 30 30))))
    (dolist (child row) (kiln-swift:ui-add stack child))
    (list* (%readout "a row") stack row)))

;; quoted symbols, not #': looked up when a story is shown, after these are defined
(defparameter *stories*
  '(("label" . %story-label) ("button" . %story-button) ("switch" . %story-switch)
    ("slider" . %story-slider) ("stepper" . %story-stepper) ("progress" . %story-progress)
    ("gauge" . %story-gauge) ("picker" . %story-picker) ("list" . %story-list) ("crown" . %story-crown)
    ("alert" . %story-alert) ("sheet" . %story-sheet) ("nav" . %story-nav) ("tabs" . %story-tabs)
    ("date" . %story-date) ("image" . %story-image) ("text" . %story-text) ("stack" . %story-stack)))

;;; ---- the book -----------------------------------------------------------------------------------

(defun %show (index)
  "Show story INDEX (modulo their number) in place of the one on show."
  (%forget *story*)
  (setf *story* nil)
  (setf *index* (mod index (length *stories*)))
  (let ((story (nth *index* *stories*)))
    (kiln-swift:ui-set-text *title* (car story))
    (setf *story* (funcall (cdr story)))))

(defun watch-storybook ()
  "Open the watch's storybook (closing any open one first), at its first story."
  (close-watch-storybook)
  (let ((background (kiln-swift:ui-panel 0 0 0 0 *background*))
        (prev (kiln-swift:ui-button "prev" 8 196 92 36))
        (next (kiln-swift:ui-button "next" 108 196 92 36)))
    (setf *title* (kiln-swift:ui-label "" *x* 30 *w* 22))
    (setf *shelf* (list background *title* prev next))
    (%on :tap prev (v) (%show (1- *index*)))
    (%on :tap next (v) (%show (1+ *index*)))
    (%show 0)
    :watch-storybook))

(defun close-watch-storybook ()
  (%forget *story*)
  (setf *story* nil)
  (%forget *shelf*)
  (setf *shelf* nil *title* nil)
  :closed)
