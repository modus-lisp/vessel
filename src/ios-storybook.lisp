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

(defun %story-segmented ()
  (let* ((titles '("one" "two" "three"))
         (picked (kiln-swift:ui-label "selected: one" 24 250 350 30))
         (control (kiln-swift:ui-segmented titles 24 200 350 32)))
    (kiln-swift:on-event :select control
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text picked (format nil "selected: ~A" (nth index titles)))))
    (list control picked)))

(defun %story-progress ()
  "A progress bar, an activity indicator, and a button that stops and starts the indicator."
  (let* ((bar (kiln-swift:ui-progress 40 24 200 350 20))
         (spinner (kiln-swift:ui-spinner 24 240 40 40))
         (state (kiln-swift:ui-label "spinning" 80 250 200 30))
         (toggle (kiln-swift:ui-button "toggle" 24 300 120 40))
         (spinning t))
    (kiln-swift:on-tap toggle
                       (lambda (value h)
                         (declare (ignore value h))
                         (setf spinning (not spinning))
                         (kiln-swift:ui-set-value spinner spinning)
                         (kiln-swift:ui-set-text state (if spinning "spinning" "stopped"))))
    (list bar spinner state toggle)))

(defun %story-textview ()
  "A multi-line text view; the count under it follows the text (a :text event on each change)."
  (let* ((initial (format nil "a multi-line text view~%second line"))
         (view (kiln-swift:ui-text-view initial 24 200 350 140))
         (tally (kiln-swift:ui-label (format nil "chars: ~D" (length initial)) 24 350 350 30)))
    (kiln-swift:on-event :text view
                         (lambda (value h)
                           (declare (ignore value))
                           (kiln-swift:ui-set-text tally
                                                   (format nil "chars: ~D" (length (kiln-swift:ui-text-value h))))))
    (list view tally)))

(defun %story-stepper ()
  (let* ((readout (kiln-swift:ui-label "value 50" 24 250 350 30))
         (stepper (kiln-swift:ui-stepper 50 24 200)))
    (kiln-swift:on-event :step stepper
                         (lambda (value h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "value ~D" (round value)))))
    (list stepper readout)))

(defun %story-pages ()
  "A page control of five pages, and a button that turns to the next one."
  (let* ((current 0)
         (readout (kiln-swift:ui-label "page 1" 24 250 350 30))
         (control (kiln-swift:ui-page-control 5 0 24 200))
         (turn (kiln-swift:ui-button "turn" 24 300 120 40)))
    (kiln-swift:on-event :page control
                         (lambda (index h)
                           (declare (ignore h))
                           (setf current index)
                           (kiln-swift:ui-set-text readout (format nil "page ~D" (1+ index)))))
    (kiln-swift:on-tap turn
                       (lambda (value h)
                         (declare (ignore value h))
                         (setf current (mod (1+ current) 5))
                         (kiln-swift:ui-set-value control current)
                         (kiln-swift:ui-set-text readout (format nil "page ~D" (1+ current)))))
    (list control readout turn)))

(defun %story-date ()
  (let* ((readout (kiln-swift:ui-label "seconds: 1000000000" 24 250 350 30))
         (picker (kiln-swift:ui-date-picker 1000000000 24 200)))
    (kiln-swift:on-event :date picker
                         (lambda (value h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "seconds: ~D" (round value)))))
    (list picker readout)))

(defun %story-hold ()
  "A long press on the label turns the readout to 'held'."
  (let* ((target (kiln-swift:ui-label "hold me" 24 200 350 60))
         (readout (kiln-swift:ui-label "not held" 24 280 350 30)))
    (kiln-swift:on-event :longpress target
                         (lambda (value h)
                           (declare (ignore value h))
                           (kiln-swift:ui-set-text readout "held")))
    (kiln-swift:ui-on-long-press target 500)
    (list target readout)))

(defun %story-alert ()
  "A button that shows an alert with two buttons; the readout says which one was chosen."
  (let* ((readout (kiln-swift:ui-label "chose: nothing" 24 250 350 30))
         (show (kiln-swift:ui-button "show alert" 24 200 160 40)))
    (kiln-swift:on-tap show
                       (lambda (value h)
                         (declare (ignore value h))
                         (let ((alert (kiln-swift:ui-alert "Delete?" "This cannot be undone.")))
                           (kiln-swift:ui-alert-action alert "Cancel")
                           (kiln-swift:ui-alert-action alert "Delete")
                           (kiln-swift:on-event :alert alert
                                                (lambda (index h2)
                                                  (declare (ignore h2))
                                                  (kiln-swift:ui-set-text
                                                   readout (format nil "chose: ~A" (if (= index 0) "Cancel" "Delete")))))
                           (kiln-swift:ui-alert-present alert))))
    (list show readout)))

(defun %story-tabs ()
  (let* ((titles '("one" "two" "three"))
         (readout (kiln-swift:ui-label "tab: one" 24 260 350 30))
         (bar (kiln-swift:ui-tab-bar titles 24 200 350 49)))
    (kiln-swift:on-event :select bar
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "tab: ~A" (nth index titles)))))
    (list bar readout)))

(defun %story-refresh ()
  "A list that refreshes when pulled down; the readout counts the refreshes, and the spinner stops at once."
  (let* ((refreshes 0)
         (readout (kiln-swift:ui-label "refreshes: 0" 24 200 230 30))
         (pull (kiln-swift:ui-button "pull" 270 196 90 36))
         (table (kiln-swift:ui-list 24 240 350 500)))
    (kiln-swift:ui-list-set-data table (loop for i from 1 to 10
                                             collect (list :title (format nil "row ~D" i))))
    (kiln-swift:on-event :refresh table
                         (lambda (value h)
                           (declare (ignore value h))
                           (incf refreshes)
                           (kiln-swift:ui-set-value table 0)
                           (kiln-swift:ui-set-text readout (format nil "refreshes: ~D" refreshes))))
    (kiln-swift:ui-on-refresh table)
    ;; a pull by hand is a drag from the top of the list; this button makes the same pull as a test does
    (kiln-swift:on-tap pull (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-send-refresh table)))
    (list table readout pull)))

(defun %story-sheet ()
  "A button that shows an action sheet; the readout says which button was chosen."
  (let* ((readout (kiln-swift:ui-label "chose: nothing" 24 250 350 30))
         (show (kiln-swift:ui-button "show sheet" 24 200 160 40)))
    (kiln-swift:on-tap show
                       (lambda (value h)
                         (declare (ignore value h))
                         (let ((sheet (kiln-swift:ui-alert "Share" "Send this note to:" :sheet t)))
                           (kiln-swift:ui-alert-action sheet "Mail")
                           (kiln-swift:ui-alert-action sheet "Messages")
                           (kiln-swift:on-event :alert sheet
                                                (lambda (index h2)
                                                  (declare (ignore h2))
                                                  (kiln-swift:ui-set-text
                                                   readout (format nil "chose: ~A" (if (= index 0) "Mail" "Messages")))))
                           (kiln-swift:ui-alert-present sheet))))
    (list show readout)))

(defun %story-search ()
  "A search bar, its text mirrored under it (a :text event on each change); 'put' sets the text from code."
  (let* ((bar (kiln-swift:ui-search-bar "find notes" 24 200 350 56))
         (readout (kiln-swift:ui-label "text: " 24 270 350 30))
         (put (kiln-swift:ui-button "put" 24 320 100 40)))
    (kiln-swift:on-event :text bar
                         (lambda (value h)
                           (declare (ignore value))
                           (kiln-swift:ui-set-text readout (format nil "text: ~A" (kiln-swift:ui-text-value h)))))
    (kiln-swift:on-tap put
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-set-text bar "hello")
                         (kiln-swift:ui-set-text readout "text: hello")))
    (list bar readout put)))

(defun %story-toolbar ()
  "A toolbar of three buttons; the readout says which one was tapped."
  (let* ((titles '("bold" "italic" "underline"))
         (readout (kiln-swift:ui-label "tapped: nothing" 24 260 350 30))
         (bar (kiln-swift:ui-toolbar titles 24 200 350 44)))
    (kiln-swift:on-event :select bar
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "tapped: ~A" (nth index titles)))))
    (list bar readout)))

(defun %story-picker ()
  "A wheel of three colours; the readout names the row chosen, and the button selects blue from code."
  (let* ((colours '("red" "green" "blue"))
         (readout (kiln-swift:ui-label "colour: red" 24 330 350 30))
         (wheel (kiln-swift:ui-picker colours 24 200 350 120))
         (choose (kiln-swift:ui-button "select blue" 24 370 130 40)))
    (kiln-swift:on-event :select wheel
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "colour: ~A" (nth index colours)))))
    (kiln-swift:on-tap choose
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-set-value wheel 2)
                         (kiln-swift:ui-set-text readout "colour: blue")))
    (list wheel readout choose)))

(defun %story-blur ()
  "A blur over a violet panel, with a label in the blur's content."
  (let* ((panel (kiln-swift:ui-panel 24 200 350 200 #x8a2be2))
         (blur (kiln-swift:ui-blur 24 260 350 80))
         (label (kiln-swift:ui-label "blurred" 40 280 200 30)))
    (kiln-swift:ui-add blur label)
    (list panel blur label)))

(defun %story-pan ()
  "A label to drag across: the readout shows the distance, when the drag ends."
  (let* ((target (kiln-swift:ui-label "drag across me" 24 200 350 60))
         (readout (kiln-swift:ui-label "dragged: none" 24 280 350 30)))
    (kiln-swift:on-event :pan target
                         (lambda (value h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "dragged: ~D" (round value)))))
    (kiln-swift:ui-on-pan target)
    (list target readout)))

(defun %story-grid ()
  "A grid of twelve tiles (a collection view); a tap on a tile names it."
  (let* ((readout (kiln-swift:ui-label "tile: none" 24 200 350 30))
         (grid (kiln-swift:ui-grid (loop for i from 1 to 12 collect (format nil "tile ~D" i)) 24 240 350 300)))
    (kiln-swift:on-event :select grid
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "tile: ~D" (1+ index)))))
    (list grid readout)))

(defun %story-menu ()
  "A button with a menu of three choices; the readout names the one chosen."
  (let* ((choices '("copy" "paste" "share"))
         (readout (kiln-swift:ui-label "chose: nothing" 24 260 350 30))
         (menu (kiln-swift:ui-menu-button choices 24 200 160 40)))
    (kiln-swift:on-event :menu menu
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "chose: ~A" (nth index choices)))))
    (list menu readout)))

(defun %story-color ()
  "A colour well, violet; the readout gives its colour, and the button sets it to green from code."
  (let* ((well (kiln-swift:ui-color-well #x8a2be2 24 200 60 40))
         (readout (kiln-swift:ui-label "color: #8A2BE2" 100 205 280 30))
         (green (kiln-swift:ui-button "set green" 24 260 130 40)))
    (kiln-swift:on-event :color well
                         (lambda (rgb h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "color: #~6,'0X" rgb))))
    (kiln-swift:on-tap green
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-set-value well #x00ff00)
                         (kiln-swift:ui-set-text readout "color: #00FF00")))
    (list well readout green)))

(defun %story-nav ()
  "A navigation bar: 'push' adds a step, whose right button ('add') counts its taps; the back button pops."
  (let* ((bar (kiln-swift:ui-nav-bar 24 200 350 44))
         (adds (kiln-swift:ui-label "adds: 0" 24 260 350 30))
         (pops (kiln-swift:ui-label "pops: 0" 24 290 350 30))
         (pusher (kiln-swift:ui-button "push" 24 330 100 40))
         (popper (kiln-swift:ui-button "pop" 140 330 100 40))
         (count-adds 0)
         (count-pops 0)
         (steps 0))
    (kiln-swift:ui-nav-push bar "home")
    (kiln-swift:on-event :navtap bar
                         (lambda (value h)
                           (declare (ignore value h))
                           (incf count-adds)
                           (kiln-swift:ui-set-text adds (format nil "adds: ~D" count-adds))))
    (kiln-swift:on-event :pop bar
                         (lambda (value h)
                           (declare (ignore value h))
                           (incf count-pops)
                           (kiln-swift:ui-set-text pops (format nil "pops: ~D" count-pops))))
    (kiln-swift:on-tap pusher
                       (lambda (value h)
                         (declare (ignore value h))
                         (incf steps)
                         (kiln-swift:ui-nav-push bar (format nil "step ~D" steps) "add")))
    (kiln-swift:on-tap popper
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-nav-pop bar)))
    (list bar adds pops pusher popper)))

(defun %story-context ()
  "A label with a context menu: a long press opens it, and the readout names the choice."
  (let* ((choices '("copy" "delete"))
         (target (kiln-swift:ui-label "hold for a menu" 24 200 350 60))
         (readout (kiln-swift:ui-label "menu: none" 24 280 350 30)))
    (kiln-swift:on-event :context target
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "menu: ~A" (nth index choices)))))
    (kiln-swift:ui-context-menu target choices)
    (list target readout)))

(defun %story-animate ()
  "A label that moves across the screen, over half a second, when 'move' is tapped."
  (let* ((mover (kiln-swift:ui-label "move me" 24 200 120 40))
         (move (kiln-swift:ui-button "move" 24 260 120 40)))
    (kiln-swift:on-tap move
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-animate-to mover 200 200 120 40 500)))
    (list mover move)))

(defun %story-shapes ()
  "An oval and a rounded rectangle, filled."
  (let ((oval (kiln-swift:ui-shape :oval 24 200 100 100 #x8a2be2))
        (rounded (kiln-swift:ui-shape :rounded 150 200 200 100 #x2ecc71))
        (caption (kiln-swift:ui-label "two shapes" 24 320 350 30)))
    (list oval rounded caption)))

(defun %story-swipe ()
  "A label to swipe to the left: the readout names the swipe."
  (let* ((target (kiln-swift:ui-label "swipe me left" 24 200 350 60))
         (readout (kiln-swift:ui-label "swiped: none" 24 280 350 30)))
    (kiln-swift:on-event :swipe target
                         (lambda (direction h)
                           (declare (ignore direction h))
                           (kiln-swift:ui-set-text readout "swiped: left")))
    (kiln-swift:ui-on-swipe target :left)
    (list target readout)))

(defun %story-share ()
  "A button that presents the share sheet; the readout says whether an activity was completed."
  (let* ((readout (kiln-swift:ui-label "share: none yet" 24 250 350 30))
         (share (kiln-swift:ui-button "share now" 24 200 120 40)))
    (kiln-swift:on-tap share
                       (lambda (value h)
                         (declare (ignore value h))
                         (let ((sheet (kiln-swift:ui-share "a note from the storybook")))
                           (kiln-swift:on-event :share sheet
                                                (lambda (completed h2)
                                                  (declare (ignore h2))
                                                  (kiln-swift:ui-set-text
                                                   readout (if (= completed 1) "shared: yes" "shared: no")))))))
    (list share readout)))

(defun %story-modal ()
  "A button that shows a sheet; its 'done' button dismisses it, and the readout says so."
  (let* ((readout (kiln-swift:ui-label "dismissed: no" 24 260 350 30))
         (open (kiln-swift:ui-button "open modal" 24 200 160 40)))
    (kiln-swift:on-tap open
                       (lambda (value h)
                         (declare (ignore value h))
                         (let* ((modal (kiln-swift:ui-modal))
                                (inside (kiln-swift:ui-label "inside the sheet" 24 60 300 30))
                                (done (kiln-swift:ui-button "done" 24 110 120 40)))
                           (kiln-swift:ui-add modal inside)
                           (kiln-swift:ui-add modal done)
                           (kiln-swift:on-tap done
                                              (lambda (value2 h2)
                                                (declare (ignore value2 h2))
                                                (kiln-swift:ui-modal-dismiss modal)
                                                (kiln-swift:ui-set-text readout "dismissed: yes")))
                           (kiln-swift:ui-modal-present modal))))
    (list open readout)))

(defun %story-a11y ()
  "A label whose accessibility label is not its text: the accessibility tree lists the label."
  (let ((label (kiln-swift:ui-label "plain text" 24 200 350 30)))
    (kiln-swift:ui-set-accessibility label "custom name")
    (list label)))

(defun %story-buzz ()
  "Three buttons, one per strength of feedback; the readout says which was felt."
  (let* ((readout (kiln-swift:ui-label "buzzed: none" 24 260 350 30))
         (light (kiln-swift:ui-button "light" 24 200 100 40))
         (medium (kiln-swift:ui-button "medium" 140 200 100 40))
         (heavy (kiln-swift:ui-button "heavy" 256 200 100 40)))
    (kiln-swift:on-tap light (lambda (value h) (declare (ignore value h))
                               (kiln-swift:ui-haptic :light)
                               (kiln-swift:ui-set-text readout "buzzed: light")))
    (kiln-swift:on-tap medium (lambda (value h) (declare (ignore value h))
                                (kiln-swift:ui-haptic :medium)
                                (kiln-swift:ui-set-text readout "buzzed: medium")))
    (kiln-swift:on-tap heavy (lambda (value h) (declare (ignore value h))
                               (kiln-swift:ui-haptic :heavy)
                               (kiln-swift:ui-set-text readout "buzzed: heavy")))
    (list light medium heavy readout)))

(defun %story-fade ()
  "A label that fades out over half a second when 'fade' is tapped; the readout says when it has."
  (let* ((target (kiln-swift:ui-label "fade me" 24 200 200 40))
         (readout (kiln-swift:ui-label "faded: no" 24 260 350 30))
         (fade (kiln-swift:ui-button "fade it" 24 300 120 40)))
    (kiln-swift:on-event :fade target
                         (lambda (value h)
                           (declare (ignore value h))
                           (kiln-swift:ui-set-text readout "faded: yes")))
    (kiln-swift:on-tap fade (lambda (value h) (declare (ignore value h))
                              (kiln-swift:ui-fade-to target 0 500)))
    (list target readout fade)))

(defun %story-paging ()
  "Three pages to swipe between; the readout names the page that comes to rest."
  (let* ((scroll (kiln-swift:ui-paging 3 24 200 350 120))
         (readout (kiln-swift:ui-label "at page 1" 24 340 350 30)))
    (dotimes (i 3)
      (kiln-swift:ui-add scroll (kiln-swift:ui-label (format nil "page ~D" (1+ i)) (+ 24 (* i 350)) 40 300 40)))
    (kiln-swift:on-event :page scroll
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "at page ~D" (1+ index)))))
    (list scroll readout)))

(defun %story-rows ()
  "A list whose rows swipe to reveal two actions; the readout names the action tapped."
  (let* ((actions '("archive" "flag"))
         (readout (kiln-swift:ui-label "action: none" 24 200 350 30))
         (table (kiln-swift:ui-list 24 240 350 400)))
    (kiln-swift:ui-list-set-data table (loop for i from 1 to 8
                                             collect (list :title (format nil "mail ~D" i))))
    (kiln-swift:ui-list-actions table actions)
    (kiln-swift:on-event :rowaction table
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "action: ~A" (nth index actions)))))
    (list table readout)))

(defun %story-announce ()
  "A button that makes VoiceOver announce a line; the readout says it was sent."
  (let* ((readout (kiln-swift:ui-label "announced: no" 24 200 350 30))
         (announce (kiln-swift:ui-button "say it" 24 240 140 40)))
    (kiln-swift:on-tap announce
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-announce "saved")
                         (kiln-swift:ui-set-text readout "announced: yes")))
    (list announce readout)))

(defun %story-tabctl ()
  "A tab bar controller, shown as a sheet.  Picking a tab is the readout's last tab; 'dismiss' closes the sheet."
  (let* ((readout (kiln-swift:ui-label "last tab: none" 24 260 350 30))
         (open (kiln-swift:ui-button "open tabs" 24 200 160 40)))
    (kiln-swift:on-tap open
                       (lambda (value h)
                         (declare (ignore value h))
                         (let ((tabs (kiln-swift:ui-tab-controller '("home" "search"))))
                           (dolist (i '(0 1))
                             (let* ((content (kiln-swift:ui-tab-content tabs i))
                                    (dismiss (kiln-swift:ui-button "dismiss" 24 110 120 40)))
                               (kiln-swift:ui-add content (kiln-swift:ui-label
                                                           (if (= i 0) "home tab" "search tab") 24 60 300 30))
                               (kiln-swift:ui-add content dismiss)
                               (kiln-swift:on-tap dismiss
                                                  (lambda (value2 h2)
                                                    (declare (ignore value2 h2))
                                                    (kiln-swift:ui-modal-dismiss tabs)))))
                           (kiln-swift:on-event :select tabs
                                                (lambda (index h2)
                                                  (declare (ignore h2))
                                                  (kiln-swift:ui-set-text
                                                   readout (format nil "last tab: ~A" (nth index '("home" "search"))))))
                           (kiln-swift:ui-modal-present tabs :large t))))
    (list open readout)))

(defun %story-navctl ()
  "A navigation controller, shown as a sheet: its root pushes a view, the back button pops it, 'dismiss' closes."
  (let* ((readout (kiln-swift:ui-label "closed: no" 24 260 350 30))
         (open (kiln-swift:ui-button "open nav" 24 200 160 40)))
    (kiln-swift:on-tap open
                       (lambda (value h)
                         (declare (ignore value h))
                         (let* ((nav (kiln-swift:ui-nav-controller))
                                (root (kiln-swift:ui-nav-root nav))
                                (push-view (kiln-swift:ui-button "push view" 24 140 140 40))
                                (dismiss (kiln-swift:ui-button "dismiss" 24 200 120 40)))
                           (kiln-swift:ui-add root (kiln-swift:ui-label "root view" 24 100 300 30))
                           (kiln-swift:ui-add root push-view)
                           (kiln-swift:ui-add root dismiss)
                           (kiln-swift:on-tap push-view
                                              (lambda (value2 h2)
                                                (declare (ignore value2 h2))
                                                (let ((pushed (kiln-swift:ui-nav-push-view nav "second")))
                                                  (kiln-swift:ui-add pushed (kiln-swift:ui-label "second view" 24 100 300 30)))))
                           (kiln-swift:on-tap dismiss
                                              (lambda (value2 h2)
                                                (declare (ignore value2 h2))
                                                (kiln-swift:ui-modal-dismiss nav)
                                                (kiln-swift:ui-set-text readout "closed: yes")))
                           (kiln-swift:ui-modal-present nav :large t))))
    (list open readout)))

(defun %story-attributed ()
  (list (kiln-swift:ui-attributed-label "underlined text" 24 200 350 30)))

(defun %story-dynamic ()
  (list (kiln-swift:ui-dynamic-label "dynamic title" 24 200 350 40)))

(defun %story-safe ()
  "The top safe-area inset: the status bar and the notch, which the screen must leave clear."
  (let ((inset (kiln-swift:ui-safe-area-top)))
    (list (kiln-swift:ui-label (if (> inset 0) "inset ok" "no inset") 24 200 350 30))))

(defun %story-parallax ()
  (let ((card (kiln-swift:ui-label "parallax card" 24 200 200 80)))
    (kiln-swift:ui-parallax card 20)
    (list card)))

(defun %story-zoom ()
  "A scroll view whose content zooms: pinch it, and the readout gives the scale when the zoom ends."
  (let* ((scroll (kiln-swift:ui-zoom-scroll 24 200 350 200))
         (readout (kiln-swift:ui-label "zoom: 100" 24 410 350 30)))
    (kiln-swift:ui-add (kiln-swift:ui-zoom-content scroll) (kiln-swift:ui-label "zoom me" 24 40 200 30))
    (kiln-swift:on-event :zoom scroll
                         (lambda (percent h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "zoom: ~D" percent))))
    (list scroll readout)))

(defun %story-drawn ()
  (list (kiln-swift:ui-drawn-image :oval #x8a2be2 120 24 200)))

;; the sixth page: attributed text, Dynamic Type, the safe area, parallax, zoom, drawing
(defparameter *stories-6*
  (list (cons "attributed" #'%story-attributed)
        (cons "dynamic" #'%story-dynamic)
        (cons "safe" #'%story-safe)
        (cons "parallax" #'%story-parallax)
        (cons "zoom" #'%story-zoom)
        (cons "drawn" #'%story-drawn)
        ;; quoted: these are defined below this list, and #' would take NIL here
        (cons "sections" '%story-sections)
        (cons "pagevc" '%story-pagevc)
        (cons "file" '%story-file)))

;; the seventh page: row editing, an edit menu, a popover, a split view
(defparameter *stories-7*
  (list (cons "editing" '%story-editing)
        (cons "editmenu" '%story-editmenu)
        (cons "popover" '%story-popover)
        (cons "split" '%story-split)
        (cons "dynamics" '%story-dynamics)
        (cons "keyboard" '%story-keyboard)
        (cons "files" '%story-files)
        (cons "collection" '%story-collection)
        (cons "photos" '%story-photos)))

;; the eighth page: a tap gesture, an edge pan, a calendar, an empty state, the colour and font pickers, input keys,
;; and the GPU
(defparameter *stories-8*
  (list (cons "tapview" '%story-tapview)
        (cons "edge" '%story-edge)
        (cons "calendar" '%story-calendar)
        (cons "empty" '%story-empty)
        (cons "colors" '%story-colors)
        (cons "fonts" '%story-fonts)
        (cons "keys" '%story-keys)
        (cons "gpu" '%story-gpu)
        (cons "speech" '%story-speech)))

(defun %story-speech ()
  "The phone's own voice and ear, end to end and without a microphone: say a sentence into a file
   (AVSpeechSynthesizer), transcribe the file (SFSpeechRecognizer), and show what was heard, normalized.
   \"speak aloud\" says the sentence through the speaker too."
  (let* ((sentence "the quick brown fox jumps over the lazy dog")
         (said (kiln-swift:ui-label "said: nothing yet" 24 170 350 30))
         (readout (kiln-swift:ui-label "heard: nothing yet" 24 200 350 60))
         (trip (kiln-swift:ui-button "round trip" 24 270 160 40))
         (aloud (kiln-swift:ui-button "speak aloud" 200 270 150 40)))
    (kiln-swift:on-tap trip
                       (lambda (value h)
                         (declare (ignore value h))
                         (kiln-swift:ui-set-text readout "heard: ...")
                         (kiln-speech:round-trip
                          sentence
                          (lambda (heard)
                            (kiln-swift:ui-set-text readout
                                                    (if (eq heard :failed)
                                                        "heard: no recognizer here"
                                                        (format nil "heard: ~A" (kiln-speech:normalize heard)))))
                          :said (lambda (ok) (kiln-swift:ui-set-text said (if ok "said: to a file" "said: failed"))))))
    (kiln-swift:on-tap aloud
                       (lambda (value h)
                         (declare (ignore value h))
                         (glass.desk:voice-say glass.desk:*voice* sentence)))
    (list said readout trip aloud)))

(defparameter *spin-shader*
  "#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; float4 colour; };
// each vertex is x, y, r, g, b; buffer 1 holds the time, in seconds
vertex V spin_vertex(uint i [[vertex_id]], constant float *v [[buffer(0)]], constant float &t [[buffer(1)]]) {
  float x = v[i * 5], y = v[i * 5 + 1], c = cos(t), s = sin(t);
  V out;
  out.position = float4(x * c - y * s, x * s + y * c, 0, 1);
  out.colour = float4(v[i * 5 + 2], v[i * 5 + 3], v[i * 5 + 4], 1);
  return out;
}
fragment float4 spin_fragment(V in [[stage_in]]) { return in.colour; }
"
  "The GPU story's shader, in Metal Shading Language: a triangle turned by the time, its colours blended.")

(defun %story-gpu ()
  "A triangle the GPU draws and turns, every frame: a pipeline built once, a uniform written each frame, a render
   pass recorded and submitted (gpu.lisp).  The readout says when it has drawn ten frames."
  (let* ((view (kiln-gpu:gpu-view 24 200 350 350))
         (readout (kiln-swift:ui-label "gpu: starting" 24 570 350 30))
         (shader (kiln-gpu:gpu-shader *spin-shader*))
         (pipeline (and shader (kiln-gpu:gpu-pipeline shader "spin_vertex" "spin_fragment")))
         (vertices (kiln-gpu:gpu-buffer (kiln-gpu:floats  0.0  0.8   1.0 0.3 0.3
                                                          -0.7 -0.5   0.3 1.0 0.3
                                                           0.7 -0.5   0.3 0.4 1.0)))
         (time (kiln-gpu:gpu-buffer 4))
         (frames 0)
         (hook nil))
    (unless pipeline (kiln-swift:ui-set-text readout "gpu: no pipeline (see the log)"))
    (setf hook
          (lambda ()
            ;; a frame: the time into its buffer, then one pass -- or, the view gone, this hook goes too
            (kiln-gpu:gpu-write time (kiln-gpu:floats (/ frames 60.0)))
            (if (and pipeline
                     (kiln-gpu:with-gpu-pass (pass view :clear #x101820)
                       (kiln-gpu:gpu-use pass pipeline)
                       (kiln-gpu:gpu-bind pass vertices 0)
                       (kiln-gpu:gpu-bind pass time 1)
                       (kiln-gpu:gpu-draw pass 3)))
                (when (= (incf frames) 10) (kiln-swift:ui-set-text readout "gpu: drawing"))
                (setf cl-user::*kiln-frame-hooks* (remove hook cl-user::*kiln-frame-hooks*)))))
    (when pipeline (push hook cl-user::*kiln-frame-hooks*))
    (list view readout)))

(defun %story-tapview ()
  "A label that a tap gesture makes tappable; the readout counts the taps."
  (let* ((count 0)
         (target (kiln-swift:ui-label "tap this label" 24 200 350 40))
         (readout (kiln-swift:ui-label "label taps: 0" 24 260 350 30)))
    (kiln-swift:ui-on-tap-gesture target)
    (kiln-swift:on-tap target (lambda (value h)
                                (declare (ignore value h))
                                (kiln-swift:ui-set-text readout (format nil "label taps: ~D" (incf count)))))
    (list target readout)))

(defun %story-edge ()
  "A panel across the screen: a pan in from its left edge is the readout's 'edge: yes'."
  (let* ((panel (kiln-swift:ui-panel 0 200 402 400 #x22262e))
         (readout (kiln-swift:ui-label "edge: no" 24 620 350 30)))
    (kiln-swift:ui-on-edge-pan panel)
    (kiln-swift:on-event :edge panel (lambda (value h)
                                       (declare (ignore value h))
                                       (kiln-swift:ui-set-text readout "edge: yes")))
    (list panel readout)))

(defun %story-calendar ()
  "A calendar of November 2026; a day picked is the readout's day."
  (let* ((readout (kiln-swift:ui-label "day: none" 24 200 350 30))
         (calendar (kiln-swift:ui-calendar 1794700800 24 240 350 400)))
    (kiln-swift:on-event :date calendar
                         (lambda (seconds h)
                           (declare (ignore h))
                           (multiple-value-bind (s m hr day)
                               (decode-universal-time (+ (round seconds) 2208988800) 0)
                             (declare (ignore s m hr))
                             (kiln-swift:ui-set-text readout (format nil "day: ~D" day)))))
    (list readout calendar)))

(defun %story-empty ()
  (list (kiln-swift:ui-unavailable "No mail" 24 200 350 300)))

(defun %story-colors ()
  "A button that shows the system colour picker; the readout says when a colour was picked."
  (let ((open (kiln-swift:ui-button "pick color" 24 200 160 40))
        (readout (kiln-swift:ui-label "color: none" 24 260 350 30)))
    (kiln-swift:on-tap open (lambda (value h)
                              (declare (ignore value h))
                              (kiln-swift:on-event :color (kiln-swift:ui-color-picker)
                                                   (lambda (rgb h2)
                                                     (declare (ignore rgb h2))
                                                     (kiln-swift:ui-set-text readout "color: picked")))))
    (list open readout)))

(defun %story-fonts ()
  "A button that shows the system font picker; the readout says when a font was picked."
  (let ((open (kiln-swift:ui-button "pick font" 24 200 160 40))
        (readout (kiln-swift:ui-label "font: none" 24 260 350 30)))
    (kiln-swift:on-tap open (lambda (value h)
                              (declare (ignore value h))
                              (kiln-swift:on-event :font (kiln-swift:ui-font-picker)
                                                   (lambda (value2 h2)
                                                     (declare (ignore value2 h2))
                                                     (kiln-swift:ui-set-text readout "font: picked")))))
    (list open readout)))

(defun %story-keys ()
  "A text field whose keyboard is an input view of three keys; the readout mirrors the field."
  (let* ((field (kiln-swift:ui-text-field "keys here" 24 200 350 36))
         (mirror (kiln-swift:ui-label "typed: nothing" 24 250 350 30)))
    (kiln-swift:ui-input-keys field '("1" "2" "3"))
    (kiln-swift:on-event :text field
                         (lambda (value h)
                           (declare (ignore value))
                           (kiln-swift:ui-set-text mirror (format nil "typed: ~A" (kiln-swift:ui-text-value h)))))
    (list field mirror)))

(defun %story-sections ()
  "A list in two sections, 'today' and 'later'; the readout names the row picked, by its number."
  (let* ((readout (kiln-swift:ui-label "selected: none" 24 200 350 30))
         (table (kiln-swift:ui-list 24 240 350 500)))
    (kiln-swift:ui-list-set-data table
                                 (loop for i from 1 to 6
                                       collect (list :title (format nil "row ~D" i)
                                                     :section (if (<= i 3) "today" "later"))))
    (kiln-swift:on-event :select table
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "selected: ~D" (1+ index)))))
    (list table readout)))

(defun %story-pagevc ()
  "A page view controller, shown as a sheet: swipe between three pages; 'done' on any page closes it."
  (let* ((readout (kiln-swift:ui-label "closed: no" 24 260 350 30))
         (open (kiln-swift:ui-button "open pages" 24 200 160 40)))
    (kiln-swift:on-tap open
                       (lambda (value h)
                         (declare (ignore value h))
                         (let ((pager (kiln-swift:ui-page-controller 3)))
                           (dotimes (i 3)
                             (let ((page (kiln-swift:ui-page-content pager i))
                                   (done (kiln-swift:ui-button "done pages" 24 200 140 40)))
                               (kiln-swift:ui-add page (kiln-swift:ui-label (format nil "pager ~D" (1+ i))
                                                                            24 100 300 30))
                               (kiln-swift:ui-add page done)
                               (kiln-swift:on-tap done
                                                  (lambda (value2 h2)
                                                    (declare (ignore value2 h2))
                                                    (kiln-swift:ui-modal-dismiss pager)
                                                    (kiln-swift:ui-set-text readout "closed: yes")))))
                           (kiln-swift:ui-modal-present pager :large t))))
    (list open readout)))

(defun %story-file ()
  "A circle drawn, saved as a PNG file, and shown from that file."
  (list (kiln-swift:ui-file-image :oval #x2ecc71 120 24 240)))

(defun %story-editing ()
  "A list whose rows can be deleted in editing mode; the readout names the row deleted, by its number."
  (let* ((readout (kiln-swift:ui-label "deleted: none" 24 200 350 30))
         (edit (kiln-swift:ui-button "edit rows" 24 240 140 40))
         (table (kiln-swift:ui-list 24 290 350 450))
         (editing nil))
    (kiln-swift:ui-list-set-data table (loop for i from 1 to 4 collect (list :title (format nil "item ~D" i))))
    (kiln-swift:on-event :delete table
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "deleted: ~D" (1+ index)))))
    (kiln-swift:on-tap edit
                       (lambda (value h)
                         (declare (ignore value h))
                         (setf editing (not editing))
                         (kiln-swift:ui-list-editing table editing)))
    (list table readout edit)))

(defun %story-editmenu ()
  "A label with an edit menu: a long press opens it, and the readout names the choice."
  (let* ((choices '("copy" "paste"))
         (target (kiln-swift:ui-label "long press for edit menu" 24 200 350 60))
         (readout (kiln-swift:ui-label "edit: none" 24 280 350 30)))
    (kiln-swift:ui-edit-menu target choices)
    (kiln-swift:on-event :edit target
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "edit: ~A" (nth index choices)))))
    (list target readout)))

(defun %story-popover ()
  "A button that shows its content as a popover (a sheet on a phone); 'done' in it closes it."
  (let* ((readout (kiln-swift:ui-label "popover: closed" 24 260 350 30))
         (source (kiln-swift:ui-button "show popover" 24 200 160 40)))
    (kiln-swift:on-tap source
                       (lambda (value h)
                         (declare (ignore value h))
                         (let* ((content (kiln-swift:ui-modal))
                                (done (kiln-swift:ui-button "done popover" 24 120 160 40)))
                           (kiln-swift:ui-add content (kiln-swift:ui-label "in the popover" 24 60 300 30))
                           (kiln-swift:ui-add content done)
                           (kiln-swift:on-tap done
                                              (lambda (value2 h2)
                                                (declare (ignore value2 h2))
                                                (kiln-swift:ui-modal-dismiss content)
                                                (kiln-swift:ui-set-text readout "popover: closed")))
                           (kiln-swift:ui-popover content source))))
    (list source readout)))

(defun %story-split ()
  "A split view of a primary and a secondary column, shown as a sheet.  On a phone the secondary shows first, and
   Back shows the primary, whose 'done split' closes the sheet."
  (let ((open (kiln-swift:ui-button "show split" 24 200 160 40)))
    (kiln-swift:on-tap open
                       (lambda (value h)
                         (declare (ignore value h))
                         (let* ((primary (kiln-swift:ui-modal))
                                (secondary (kiln-swift:ui-modal))
                                (done (kiln-swift:ui-button "done split" 24 120 160 40))
                                (split nil))
                           (kiln-swift:ui-add primary (kiln-swift:ui-label "primary view" 24 60 300 30))
                           (kiln-swift:ui-add primary done)
                           (kiln-swift:ui-add secondary (kiln-swift:ui-label "secondary view" 24 60 300 30))
                           (kiln-swift:on-tap done
                                              (lambda (value2 h2)
                                                (declare (ignore value2 h2))
                                                (kiln-swift:ui-modal-dismiss split)))
                           (setf split (kiln-swift:ui-split primary secondary)))))
    (list open)))

(defun %story-dynamics ()
  "A label that falls under gravity when 'drop' is tapped, and comes to rest on the bottom of the screen."
  (let* ((ball (kiln-swift:ui-label "drop me" 24 200 200 40))
         (drop (kiln-swift:ui-button "drop" 24 260 120 40)))
    (kiln-swift:on-tap drop (lambda (value h)
                              (declare (ignore value h))
                              (kiln-swift:ui-gravity-drop ball)))
    (list ball drop)))

(defun %story-keyboard ()
  "A field at the bottom that lifts when the keyboard comes up (posted by the buttons, as UIKit posts it) and
   returns when it goes down."
  ;; 120 points above the bottom of the screen, whatever its height: under a 300-point keyboard on a phone or a tablet
  (let* ((field (kiln-swift:ui-label "bottom field" 24 (- (kiln-swift:ui-screen-height) 120) 350 40))
         (raise (kiln-swift:ui-button "raise keyboard" 24 200 160 40))
         (lower (kiln-swift:ui-button "lower keyboard" 24 250 160 40)))
    (kiln-swift:ui-keyboard-avoid field)
    (kiln-swift:on-tap raise (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-keyboard-post 300)))
    (kiln-swift:on-tap lower (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-keyboard-post 0)))
    (list field raise lower)))

(defun %story-files ()
  "A button that opens the system's document picker; its Cancel closes it."
  (let ((open (kiln-swift:ui-button "open files" 24 200 160 40)))
    (kiln-swift:on-tap open (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-document-picker)))
    (list open)))

(defun %story-collection ()
  "A collection view of two sections, 'alpha' and 'beta', in three columns; the readout names the tile tapped, by its
   number among all the tiles."
  (let* ((readout (kiln-swift:ui-label "tile: none" 24 200 350 30))
         (view (kiln-swift:ui-collection
                (loop for i from 1 to 6
                      collect (list :title (format nil "~A~D" (if (<= i 3) "a" "b") (if (<= i 3) i (- i 3)))
                                    :section (if (<= i 3) "alpha" "beta")))
                24 240 350 500)))
    (kiln-swift:on-event :select view
                         (lambda (index h)
                           (declare (ignore h))
                           (kiln-swift:ui-set-text readout (format nil "tile: ~D" (1+ index)))))
    (list view readout)))

(defun %story-photos ()
  "A button that opens the system's photo picker; its Cancel closes it."
  (let ((open (kiln-swift:ui-button "pick photo" 24 200 160 40)))
    (kiln-swift:on-tap open (lambda (value h) (declare (ignore value h)) (kiln-swift:ui-image-picker)))
    (list open)))

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

;; the second page of the shelf (the first page is full: nine stories)
(defparameter *stories-2*
  (list (cons "segment" #'%story-segmented)
        (cons "progress" #'%story-progress)
        (cons "textview" #'%story-textview)
        (cons "stepper" #'%story-stepper)
        (cons "pages" #'%story-pages)
        (cons "date" #'%story-date)
        (cons "hold" #'%story-hold)
        (cons "alert" #'%story-alert)
        (cons "tabs" #'%story-tabs)
        (cons "refresh" #'%story-refresh)))

;; the third page: a search bar, a toolbar, an action sheet
(defparameter *stories-3*
  (list (cons "sheet" #'%story-sheet)
        (cons "search" #'%story-search)
        (cons "toolbar" #'%story-toolbar)
        (cons "picker" #'%story-picker)
        (cons "blur" #'%story-blur)
        (cons "pan" #'%story-pan)
        (cons "grid" #'%story-grid)
        (cons "menu" #'%story-menu)
        (cons "color" #'%story-color)))

;; the pages of the shelf, in order; "next" turns to the next one
;; the fourth page: navigation, context menus, animation, shapes, swipes
(defparameter *stories-4*
  (list (cons "nav" #'%story-nav)
        (cons "context" #'%story-context)
        (cons "animate" #'%story-animate)
        (cons "shapes" #'%story-shapes)
        (cons "swipe" #'%story-swipe)
        (cons "share" #'%story-share)
        (cons "modal" #'%story-modal)
        (cons "a11y" #'%story-a11y)
        (cons "buzz" #'%story-buzz)
        (cons "fade" #'%story-fade)))

;; the fifth page: paging, swipe actions on rows, an announcement
(defparameter *stories-5*
  (list (cons "paging" #'%story-paging)
        (cons "rows" #'%story-rows)
        (cons "announce" #'%story-announce)
        (cons "tabctl" #'%story-tabctl)
        (cons "navctl" #'%story-navctl)))

(defparameter *pages* (list *stories* *stories-2* *stories-3* *stories-4* *stories-5* *stories-6* *stories-7* *stories-8*))

;;; ---- the storybook ------------------------------------------------------------------------------

(defvar *page* 1 "The shelf's page: 1, 2 or 3 (see *pages*).")
(defvar *story-buttons* nil "The shelf's story buttons, for the page on show.")
(defvar *more* nil "The shelf's page button, 'next', which turns to the next page.")

(defun %show (name)
  "Show the story NAME in place of the one on show."
  ;; a story starts with the keyboard down: a sheet that closes hands the first responder back to the desk, which
  ;; takes keys, and its keyboard would cover the bottom of the story
  (kiln-swift:ui-resign)
  (%forget *story*)
  (setf *story* nil)
  (setf *story-name* name)
  (setf *story* (funcall (cdr (assoc name (apply #'append *pages*) :test #'string=)))))

(defun %page-stories (page)
  (nth (1- page) *pages*))

(defun %shelf-stories (stories)
  "The story buttons for STORIES: five to a row, the rows below the title."
  (loop for story in stories
        for i from 0
        collect (let* ((name (car story))
                       (button (kiln-swift:ui-button name (+ 24 (* 76 (mod i 5))) (+ 120 (* 44 (floor i 5)))
                                                     68 36)))
                  (kiln-swift:on-tap button (lambda (value h) (declare (ignore value h)) (%show name)))
                  button)))

(defun %show-page (page)
  "Put PAGE's story buttons on the shelf, and show its first story."
  (%forget *story-buttons*)
  (setf *page* page)
  (setf *story-buttons* (%shelf-stories (%page-stories page)))
  (%show (car (first (%page-stories page)))))

(defun storybook ()
  "Open the storybook (closing any open one first)."
  (close-storybook)
  ;; the storybook covers the desk, whose Terminal (the Actions grid opens one) keeps the keyboard up
  (kiln-swift:ui-resign)
  (let ((background (kiln-swift:ui-panel 0 0 0 0 *background*))
        (title (kiln-swift:ui-label "storybook" 24 60 250 30)))
    (setf *shelf* (list background title))
    ;; beside the title: the other page of stories
    (setf *more* (kiln-swift:ui-button "next" 150 60 70 36))
    (push *more* *shelf*)
    (kiln-swift:on-tap *more* (lambda (value h) (declare (ignore value h))
                                 (%show-page (1+ (mod *page* (length *pages*))))))
    ;; top right: the keyboard covers the bottom of the screen while a text field has focus
    (let ((closer (kiln-swift:ui-button "close" 290 60 90 36)))
      (push closer *shelf*)
      (kiln-swift:on-tap closer (lambda (value h) (declare (ignore value h)) (close-storybook))))
    (%show-page 1)
    :storybook))

(defun close-storybook ()
  "Close the storybook: its views leave the screen, and the desk shows again."
  (%forget *story*)
  (setf *story* nil)
  (%forget *story-buttons*)
  (setf *story-buttons* nil)
  (%forget *shelf*)
  (setf *shelf* nil)
  (setf *more* nil)
  (setf *story-name* nil)
  :closed)
