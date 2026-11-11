;;;; ios-swift.lisp -- Lisp calls Swift.  A Swift function is named in host/ios/modus-swift.m's table
;;;; (by the Swift side, in host/ios/swift/), and SWIFT-CALL calls it by that name with words: integers, and the
;;;; addresses of byte arrays that hold UTF-8 text or an encoded term.  The native components are made in Swift
;;;; and driven from here.  The data Lisp sends a list is a TERM (net/term.lisp, the wire format), and the events
;;;; Swift sends back are terms too, decoded here by the same decoder as the actor messages.  SWIFT-PUMP runs
;;;; each frame (the app's frame hooks).
;;;;
;;;;   (ui-label "text" x y w h)              (ui-button "title" x y w h)
;;;;   (ui-switch on x y)                     (ui-slider value x y w h)
;;;;   (ui-text-field "placeholder" x y w h)  (ui-panel x y w h rgb)   ; w 0: the whole screen
;;;;   (ui-stack vertical-p spacing x y w h)  (ui-scroll x y w h)  (ui-image "star.fill" x y w h)
;;;;   (ui-add parent child)                  (ui-list x y w h)  (ui-list-set-data h rows)
;;;;   (ui-set-text h "text")  (ui-set-value h v)  (ui-text-value h)  (ui-set-frame h x y w h)
;;;;   (ui-remove h)  (ui-tap h)  (ui-resign)
;;;;   (on-event kind h (lambda (value h) ...))   ; kind :tap :switch :slider :text :select

(defpackage #:kiln-swift
  (:use #:cl)
  (:export #:swift-call #:ui-label #:ui-button #:ui-switch #:ui-slider #:ui-text-field #:ui-panel
           #:ui-text-view #:ui-segmented #:ui-progress #:ui-spinner
           #:ui-stepper #:ui-page-control #:ui-date-picker #:ui-on-long-press
           #:ui-alert #:ui-alert-action #:ui-alert-present #:ui-tab-bar #:ui-on-refresh #:ui-send-refresh
           #:ui-search-bar #:ui-toolbar #:ui-picker #:ui-blur #:ui-on-pan
           #:ui-grid #:ui-menu-button #:ui-color-well
           #:ui-nav-bar #:ui-nav-push #:ui-nav-pop #:ui-context-menu #:ui-animate-to #:ui-shape #:ui-on-swipe
           #:ui-share #:ui-set-accessibility #:ui-haptic #:ui-fade-to #:ui-modal #:ui-modal-present #:ui-modal-dismiss
           #:ui-paging #:ui-list-actions #:ui-announce
           #:ui-tab-controller #:ui-tab-content #:ui-nav-controller #:ui-nav-root #:ui-nav-push-view
           #:ui-attributed-label #:ui-dynamic-label #:ui-safe-area-top #:ui-parallax #:ui-zoom-scroll #:ui-zoom-content
           #:ui-drawn-image #:ui-file-image #:ui-page-controller #:ui-page-content
           #:ui-list-editing #:ui-edit-menu #:ui-popover #:ui-split #:ui-gravity-drop
           #:ui-keyboard-avoid #:ui-keyboard-post #:ui-document-picker #:ui-collection #:ui-image-picker
           #:ui-on-tap-gesture #:ui-on-edge-pan #:ui-calendar #:ui-unavailable #:ui-color-picker #:ui-font-picker
           #:ui-input-keys #:ui-gauge #:ui-crown #:ui-screen-height #:ui-screen-width
           #:ui-stack #:ui-scroll #:ui-image #:ui-add #:ui-list #:ui-list-set-data
           #:ui-set-text #:ui-set-value #:ui-text-value #:ui-set-frame #:ui-remove #:ui-tap #:ui-resign
           #:on-event #:on-tap #:swift-pump))

(in-package #:kiln-swift)

(defparameter *frame-bytes* 256)
(defparameter *name-at* 128)

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun %octets (text)
  (coerce (seal:utf8-encode text) '(simple-array (unsigned-byte 8) (*))))

(defun %term-octets (term)
  "TERM encoded as the wire bytes, in an array of exactly their size."
  (let* ((n (cl-user::term-size term))
         (b (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (cl-user::term-encode term (%addr b))
    b))

(defun %put-word (f off w)
  (dotimes (i 8) (setf (aref f (+ off i)) (ldb (byte 8 (* 8 i)) w))))

(defun %get-word (f off)
  (let ((w 0))
    (dotimes (i 8 w)
      (setf w (+ w (ash (aref f (+ off i)) (* 8 i)))))))

(defun swift-call (name &rest args)
  "Call the Swift function NAME with ARGS (at most six words); the word it returns."
  (when (> (length args) 6) (error "swift-call: at most six arguments, not ~D" (length args)))
  (let ((f (make-array *frame-bytes* :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for c across name for i from *name-at*
          do (setf (aref f i) (logand (char-code c) #xff)))
    (%put-word f 0 (length args))
    (loop for a in args for i from 0 do (%put-word f (+ 8 (* 8 i)) (or a 0)))
    (let ((rc (cl-user::%kiln-sys 1060 (%addr f) 0 0)))
      (when (< rc 0) (error "swift-call: no Swift function named ~A" name))
      (%get-word f 64))))

;;; The components.  A handle is the Swift side's name for a view.

(defun ui-label (text x y w h)
  (let ((b (%octets text)))
    (swift-call "ui_label_new" (%addr b) (length b) x y w h)))

(defun ui-button (title x y w h)
  (let ((b (%octets title)))
    (swift-call "ui_button_new" (%addr b) (length b) x y w h)))

(defun ui-switch (on x y)
  (swift-call "ui_switch_new" (if on 1 0) x y))

(defun ui-slider (value x y w h)
  (swift-call "ui_slider_new" value x y w h))

(defun ui-text-field (placeholder x y w h)
  (let ((b (%octets placeholder)))
    (swift-call "ui_text_field_new" (%addr b) (length b) x y w h)))

(defun ui-text-view (text x y w h)
  "A multi-line text view with TEXT.  Its changes are :text events (read it with UI-TEXT-VALUE)."
  (let ((b (%octets text)))
    (swift-call "ui_text_view_new" (%addr b) (length b) x y w h)))

(defun ui-segmented (titles x y w h)
  "A segmented control: one segment per string in TITLES.  Choosing one is a :select event with its index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_segmented_new" (%addr b) (length b) x y w h)))

(defun ui-progress (percent x y w h)
  "A progress bar at PERCENT (0 to 100); UI-SET-VALUE moves it."
  (swift-call "ui_progress_new" percent x y w h))

(defun ui-spinner (x y w h)
  "An activity indicator, spinning.  UI-SET-VALUE with NIL stops it, and with T starts it again."
  (swift-call "ui_spinner_new" x y w h))

(defun ui-stepper (value x y)
  "A stepper from 0 to 100, at VALUE.  Each change is a :step event with its value."
  (swift-call "ui_stepper_new" value x y))

(defun ui-page-control (pages current x y)
  "A page control: PAGES dots with the CURRENT one lit (UI-SET-VALUE moves it).  A tap on a dot is a :page
   event with its index."
  (swift-call "ui_page_new" pages current x y))

(defun ui-date-picker (seconds x y)
  "A date picker showing the date SECONDS (since 1970).  A change is a :date event with the new seconds."
  (swift-call "ui_date_new" seconds x y))

(defun ui-on-long-press (handle ms)
  "A long press on the view HANDLE, held at least MS milliseconds, is a :longpress event with HANDLE."
  (swift-call "ui_long_press_on" handle ms))

(defun ui-alert (title message &key sheet)
  "An alert with TITLE and MESSAGE, not shown yet: add its buttons with UI-ALERT-ACTION, then show it with
   UI-ALERT-PRESENT.  SHEET makes it an action sheet (its buttons from the bottom of the screen).  A button is
   an :alert event with the alert's handle and the button's index."
  (let ((tb (%octets title))
        (mb (%octets message)))
    (swift-call "ui_alert_new" (%addr tb) (length tb) (%addr mb) (length mb) (if sheet 1 0))))

(defun ui-search-bar (placeholder x y w h)
  "A search bar with PLACEHOLDER.  Each change of its text is a :text event (read it with UI-TEXT-VALUE)."
  (let ((b (%octets placeholder)))
    (swift-call "ui_search_bar_new" (%addr b) (length b) x y w h)))

(defun ui-toolbar (titles x y w h)
  "A toolbar with a button per string in TITLES, spaced evenly.  Tapping one is a :select event with its index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_toolbar_new" (%addr b) (length b) x y w h)))

(defun ui-alert-action (alert title)
  "A button of ALERT, titled TITLE, in the order added."
  (let ((b (%octets title)))
    (swift-call "ui_alert_action" alert (%addr b) (length b))))

(defun ui-alert-present (alert)
  "Show ALERT over the app's screen."
  (swift-call "ui_alert_present" alert))

(defun ui-tab-bar (titles x y w h)
  "A tab bar, an item per string in TITLES.  Choosing one is a :select event with its index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_tab_bar_new" (%addr b) (length b) x y w h)))

(defun ui-on-refresh (list-handle)
  "Pulling the list LIST-HANDLE down is a :refresh event with its handle.  UI-SET-VALUE with 0 stops its spinner."
  (swift-call "ui_refresh_on" list-handle))

(defun ui-send-refresh (list-handle)
  "A pull on the list LIST-HANDLE, sent as its refresh control sends it: the :refresh event then comes back.
   For tests, like UI-TAP."
  (swift-call "ui_send_refresh" list-handle))

(defun ui-picker (titles x y w h)
  "A wheel of rows, one per string in TITLES.  Choosing a row is a :select event with its index; UI-SET-VALUE
   selects a row from code."
  (let ((b (%term-octets titles)))
    (swift-call "ui_picker_new" (%addr b) (length b) x y w h)))

(defun ui-blur (x y w h)
  "A blur of what is behind it.  Its children (UI-ADD) go on top of the blur."
  (swift-call "ui_blur_new" x y w h))

(defun ui-on-pan (handle)
  "A pan on the view HANDLE: when it ends, a :pan event with the horizontal distance dragged."
  (swift-call "ui_pan_on" handle))

(defun ui-grid (titles x y w h)
  "A grid of tiles, one per string in TITLES.  Tapping one is a :select event with its index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_grid_new" (%addr b) (length b) x y w h)))

(defun ui-menu-button (choices x y w h)
  "A button, 'options', whose menu offers the strings in CHOICES.  Choosing one is a :menu event with its index."
  (let ((b (%term-octets choices)))
    (swift-call "ui_menu_button_new" (%addr b) (length b) x y w h)))

(defun ui-color-well (rgb x y w h)
  "A colour well showing RGB (0xRRGGBB).  A new colour is a :color event with it as RGB; UI-SET-VALUE sets it."
  (swift-call "ui_color_well_new" rgb x y w h))

(defun ui-nav-bar (x y w h)
  "A navigation bar, with no items.  UI-NAV-PUSH pushes one; the back button it then shows is a :pop event."
  (swift-call "ui_nav_bar_new" x y w h))

(defun ui-nav-push (bar title &optional right)
  "Push an item titled TITLE onto BAR.  RIGHT, if given, is its right button: tapping it is a :navtap event with
   BAR's handle."
  (let ((tb (%octets title))
        (rb (%octets (or right ""))))
    (swift-call "ui_nav_push" bar (%addr tb) (length tb) (%addr rb) (length rb))))

(defun ui-nav-pop (bar)
  "Pop the top item of BAR."
  (swift-call "ui_nav_pop" bar))

(defun ui-context-menu (handle choices)
  "A context menu on the view HANDLE, of the strings in CHOICES: a long press opens it, and a choice is a :context
   event with HANDLE and the choice's index."
  (let ((b (%term-octets choices)))
    (swift-call "ui_context_menu_on" handle (%addr b) (length b))))

(defun ui-animate-to (handle x y w h ms)
  "The view HANDLE's frame moves to X Y W H over MS milliseconds."
  (swift-call "ui_animate_to" handle x y w h ms))

(defun ui-shape (kind x y w h rgb)
  "A filled shape in the frame: KIND :oval or :rounded, in RGB (0xRRGGBB)."
  (swift-call "ui_shape_new" (if (eq kind :oval) 0 1) x y w h rgb))

(defun ui-on-swipe (handle direction)
  "A swipe on the view HANDLE in DIRECTION (:left, :right, :up or :down) is a :swipe event with HANDLE and the
   direction's number: 0 left, 1 right, 2 up, 3 down."
  (swift-call "ui_swipe_on" handle (ecase direction (:left 0) (:right 1) (:up 2) (:down 3))))

(defun ui-share (text)
  "The share sheet, with TEXT as its item.  Returns its handle: its completion is a :share event with the handle and
   1 (an activity was completed) or 0."
  (let ((b (%octets text)))
    (swift-call "ui_share_present" (%addr b) (length b))))

(defun ui-set-accessibility (handle text)
  "The accessibility label of the view HANDLE: what VoiceOver reads, and what the accessibility tree lists."
  (let ((b (%octets text)))
    (swift-call "ui_set_accessibility" handle (%addr b) (length b))))

(defun ui-haptic (strength)
  "An impact feedback: STRENGTH :light, :medium or :heavy."
  (swift-call "ui_haptic" (ecase strength (:light 0) (:medium 1) (:heavy 2))))

(defun ui-fade-to (handle percent ms)
  "The view HANDLE's alpha moves to PERCENT (0 to 100) over MS milliseconds.  Its end is a :fade event with HANDLE."
  (swift-call "ui_fade_to" handle percent ms))

(defun ui-modal ()
  "A sheet's content, not yet shown: add to it with UI-ADD, then show it with UI-MODAL-PRESENT."
  (swift-call "ui_modal_new"))

(defun ui-modal-present (modal &key large)
  "Show MODAL as a sheet, at medium height, or at large with LARGE.  Dragging it down is a :dismiss event."
  (swift-call "ui_modal_present" modal (if large 1 0)))

(defun ui-modal-dismiss (modal)
  "Dismiss MODAL from code (no :dismiss event)."
  (swift-call "ui_modal_dismiss" modal))

(defun ui-paging (pages x y w h)
  "A scroll view of PAGES pages, each W wide, that snap to a page.  Each page's content is added with UI-ADD, at
   its own x.  A page that comes to rest is a :page event with its index."
  (swift-call "ui_paging_new" pages x y w h))

(defun ui-list-actions (list-handle titles)
  "Swipe actions for the rows of the list LIST-HANDLE: a button per string in TITLES.  Tapping one is a :rowaction
   event with the list's handle and the button's index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_list_actions" list-handle (%addr b) (length b))))

(defun ui-announce (text)
  "VoiceOver announces TEXT (nothing changes on the screen)."
  (let ((b (%octets text)))
    (swift-call "ui_announce" (%addr b) (length b))))

(defun ui-tab-controller (titles)
  "A tab bar controller with a tab per string in TITLES.  Its tabs' views come from UI-TAB-CONTENT; show it with
   UI-MODAL-PRESENT.  A tab picked is a :select event with its index."
  (let ((b (%term-octets titles)))
    (swift-call "ui_tab_controller_new" (%addr b) (length b))))

(defun ui-tab-content (tabs index)
  "The view of tab INDEX of TABS, to add to with UI-ADD."
  (swift-call "ui_tab_content" tabs index))

(defun ui-nav-controller ()
  "A navigation controller, its root view from UI-NAV-ROOT.  Show it with UI-MODAL-PRESENT; UI-NAV-PUSH-VIEW pushes
   a view (its back button reads 'back')."
  (swift-call "ui_nav_controller_new"))

(defun ui-nav-root (nav)
  "The view of the root of the navigation controller NAV."
  (swift-call "ui_nav_root" nav))

(defun ui-nav-push-view (nav title)
  "Push a view titled TITLE onto NAV; its handle is returned, to add to with UI-ADD."
  (let ((b (%octets title)))
    (swift-call "ui_nav_push_view" nav (%addr b) (length b))))

(defun ui-attributed-label (text x y w h)
  "A label of attributed text: TEXT, underlined and tinted."
  (let ((b (%octets text)))
    (swift-call "ui_attributed_label_new" (%addr b) (length b) x y w h)))

(defun ui-dynamic-label (text x y w h)
  "A label in a text style that follows the user's text size (Dynamic Type)."
  (let ((b (%octets text)))
    (swift-call "ui_dynamic_label_new" (%addr b) (length b) x y w h)))

(defun ui-safe-area-top ()
  "The top safe-area inset of the screen, in points."
  (swift-call "ui_safe_area_top"))

(defun ui-screen-height ()
  "The height of the app's screen, in points."
  (swift-call "ui_screen_height"))

(defun ui-screen-width ()
  "The width of the app's screen, in points."
  (swift-call "ui_screen_width"))

(defun ui-parallax (handle amount)
  "A parallax motion effect on the view HANDLE: it moves with the device's tilt, by AMOUNT points."
  (swift-call "ui_parallax_on" handle amount))

(defun ui-zoom-scroll (x y w h)
  "A scroll view whose content zooms from 1x to 4x.  Its content view is UI-ZOOM-CONTENT; a zoom that ends is a
   :zoom event with the scale in percent."
  (swift-call "ui_zoom_scroll_new" x y w h))

(defun ui-zoom-content (scroll)
  "The content view of the zoom scroll view SCROLL, to add to with UI-ADD."
  (swift-call "ui_zoom_content" scroll))

(defun ui-drawn-image (kind rgb size x y)
  "An image drawn here, an oval (KIND :oval) or a rounded square, in RGB, SIZE points square at X Y."
  (swift-call "ui_drawn_image_new" (if (eq kind :oval) 0 1) rgb size x y))

(defun ui-file-image (kind rgb size x y)
  "A shape (KIND :oval or :rounded, in RGB) drawn, saved as a PNG file, and shown from that file."
  (swift-call "ui_file_image_new" (if (eq kind :oval) 0 1) rgb size x y))

(defun ui-page-controller (count)
  "A page view controller of COUNT pages.  Its pages' views come from UI-PAGE-CONTENT; show it with UI-MODAL-PRESENT.
   A page that comes to rest is a :page event with its index."
  (swift-call "ui_page_controller_new" count))

(defun ui-page-content (pager index)
  "The view of page INDEX of the page view controller PAGER, to add to with UI-ADD."
  (swift-call "ui_page_content" pager index))

(defun ui-list-editing (list-handle on)
  "The rows of the list LIST-HANDLE show their delete controls when ON.  Deleting one is a :delete event with the
   row's index."
  (swift-call "ui_list_editing" list-handle (if on 1 0)))

(defun ui-edit-menu (handle choices)
  "A long press on the view HANDLE opens an edit menu of the strings in CHOICES.  Choosing one is an :edit event
   with HANDLE and the choice's index."
  (let ((b (%term-octets choices)))
    (swift-call "ui_edit_menu_on" handle (%addr b) (length b))))

(defun ui-popover (modal source)
  "Show MODAL (from UI-MODAL) as a popover from the view SOURCE.  On a phone it is a sheet."
  (swift-call "ui_popover_present" modal source))

(defun ui-split (primary secondary)
  "A split view of two contents (from UI-MODAL), shown as a sheet.  On a phone it shows the primary."
  (swift-call "ui_split_present" primary secondary))

(defun ui-on-tap-gesture (handle)
  "A tap on the view HANDLE (any view, by a tap gesture recognizer) is a :tap event, as a button's is."
  (swift-call "ui_tap_on" handle))

(defun ui-on-edge-pan (handle)
  "A pan in from the screen's left edge over the view HANDLE is an :edge event."
  (swift-call "ui_edge_pan_on" handle))

(defun ui-calendar (seconds x y w h)
  "A calendar showing the month of the date SECONDS (since 1970); a day picked is a :date event with its seconds."
  (swift-call "ui_calendar_new" seconds x y w h))

(defun ui-unavailable (text x y w h)
  "A content-unavailable view: an image and TEXT, as an empty screen shows."
  (let ((b (%octets text)))
    (swift-call "ui_unavailable_new" (%addr b) (length b) x y w h)))

(defun ui-color-picker ()
  "The system colour picker, over the app.  Returns its handle: a colour picked is a :color event with it as RGB."
  (swift-call "ui_color_picker_show"))

(defun ui-font-picker ()
  "The system font picker, over the app.  Returns its handle: a font picked is a :font event."
  (swift-call "ui_font_picker_show"))

(defun ui-input-keys (field keys)
  "The text field FIELD's keyboard is an input view of a key per string in KEYS; a key types its string."
  (let ((b (%term-octets keys)))
    (swift-call "ui_input_keys_on" field (%addr b) (length b))))

(defun ui-image-picker ()
  "The system's photo picker over the app.  Its Cancel closes it."
  (swift-call "ui_image_picker_show"))

(defun ui-collection (rows x y w h)
  "A collection view of sections, in compositional layout: ROWS as UI-LIST-SET-DATA (each a plist with :TITLE and
   :SECTION), three tiles to a row under a header per section.  A tap on a tile is a :select event with the row's
   index among all the rows."
  (let ((b (%term-octets rows)))
    (swift-call "ui_collection_new" (%addr b) (length b) x y w h)))

(defun ui-keyboard-avoid (handle)
  "The view HANDLE moves up out of the keyboard's way when the keyboard comes up, and returns when it goes down."
  (swift-call "ui_keyboard_avoid_on" handle))

(defun ui-keyboard-post (height)
  "Post the keyboard notifications for a keyboard HEIGHT points tall (0: it goes down).  For tests."
  (swift-call "ui_keyboard_post" height))

(defun ui-document-picker ()
  "The system's document picker, for opening a text file, shown over the app.  Its Cancel closes it."
  (swift-call "ui_document_picker_show"))

(defun ui-gravity-drop (handle)
  "The view HANDLE falls under gravity, until it comes to rest on the bottom of its superview (UIDynamicAnimator)."
  (swift-call "ui_gravity_drop" handle))

(defun ui-gauge (value label x y w h)
  "The watch's gauge: a ring at VALUE (0 to 100), named LABEL.  UI-SET-VALUE moves it.  (watchOS only.)"
  (let ((gauge (swift-call "ui_gauge_new" value x y w h)))
    (ui-set-text gauge label)
    gauge))

(defun ui-crown (label x y w h)
  "The watch's Digital Crown, turning a value from 0 to 100 shown after LABEL.  Each change is a :crown event
   with the value.  (watchOS only.)"
  (let ((b (%octets label)))
    (swift-call "ui_crown_new" (%addr b) (length b) x y w h)))

(defun ui-panel (x y w h rgb)
  (swift-call "ui_panel_new" x y w h rgb))

(defun ui-stack (vertical-p spacing x y w h)
  "A stack: its children laid out in a column (VERTICAL-P) or a row, SPACING points apart."
  (swift-call "ui_stack_new" (if vertical-p 0 1) spacing x y w h))

(defun ui-scroll (x y w h)
  "A scroll view: the children added to it stack vertically and scroll."
  (swift-call "ui_scroll_new" x y w h))

(defun ui-image (name x y w h)
  "An SF Symbol by NAME (\"star.fill\"), in white."
  (let ((b (%octets name)))
    (swift-call "ui_image_new" (%addr b) (length b) x y w h)))

(defun ui-add (parent child)
  "Put CHILD into PARENT: a stack's arranged children, a scroll view's content, or a view's subviews."
  (swift-call "ui_add" parent child))

(defun ui-list (x y w h)
  "A list of rows: set its rows with UI-LIST-SET-DATA.  The table makes its cells as they scroll into view; a
   tap on a row is a :select event with the row's index."
  (swift-call "ui_list_new" x y w h))

(defun ui-list-set-data (handle rows)
  "ROWS: a list of plists, each (:title \"...\" :subtitle \"...\") -- the list's rows, in order, as a term."
  (let ((b (%term-octets rows)))
    (swift-call "ui_list_set_data" handle (%addr b) (length b))))

(defun ui-set-text (handle text)
  (let ((b (%octets text)))
    (swift-call "ui_set_text" handle (%addr b) (length b))))

(defun ui-set-value (handle value)
  "A switch's state (T or NIL), or a slider's value."
  (swift-call "ui_set_value" handle (if (eq value t) 1 (if (null value) 0 value))))

(defun ui-text-value (handle)
  "The text of a text field (or label), as a string."
  (let* ((cap 1024)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (swift-call "ui_text_value" handle (%addr buf) cap)))
    (cond ((zerop n) "")
          ((> n cap)
           (let ((big (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
             (swift-call "ui_text_value" handle (%addr big) n)
             (seal:utf8-decode big :end n)))
          (t (seal:utf8-decode buf :end n)))))

(defun ui-set-frame (handle x y w h)
  (swift-call "ui_set_frame" handle x y w h))

(defun ui-remove (handle)
  (swift-call "ui_remove" handle))

(defun ui-tap (handle)
  "Tap the button HANDLE, as a touch would: its event comes back through SWIFT-PUMP."
  (swift-call "ui_send_tap" handle))

(defun ui-resign ()
  "The keyboard goes away (a text field's, or the desk's)."
  (swift-call "ui_resign"))

;;; The events.  Each is a term: (:TAP h), (:SWITCH h on), (:SLIDER h value), (:TEXT h), (:SELECT h index).

(defun %next-event ()
  "The oldest event from the native side, as its term: (kind handle [value]), or NIL when there is none."
  (let* ((cap 1024)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (swift-call "ui_events_next" (%addr buf) cap)))
    (when (> n 0)
      ;; the decoder reads from the event's bytes: point its cursor (the one it reads, the world's) at them
      (setf (cl-user::mem-ref (cl-user::decode-ptr-addr) :u64) (%addr buf))
      (cl-user::term-decode-step))))

(defvar *handlers* (make-hash-table :test 'equal))

(defun on-event (kind handle fn)
  "Run FN with (value handle) when HANDLE sends an event of KIND (:tap, :switch, :slider, :text or :select)."
  (setf (gethash (list kind handle) *handlers*) fn)
  (pushnew 'swift-pump cl-user::*kiln-frame-hooks*))

(defun on-tap (handle fn)
  (on-event :tap handle fn))

(defun swift-pump ()
  "Each frame: the native events, delivered (a few at a time, so a frame is never long)."
  (loop repeat 16
        do (let ((event (%next-event)))
             (unless event (return))
             (destructuring-bind (kind handle &optional value) event
               (let ((fn (gethash (list kind handle) *handlers*)))
                 (when fn
                   (handler-case (funcall fn value handle)
                     (serious-condition (e)
                       (format t "~&event handler ~A ~A: ~A~%" kind handle e)))))))))
