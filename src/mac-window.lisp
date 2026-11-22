;;;; mac-window.lisp -- the modus window on a Mac, from Lisp: its title, its chrome, its size.  The Swift side is
;;;; modus's host/macos/swift/MacWindow.swift (window_*), called through ios-swift.lisp's SWIFT-CALL.  The app's
;;;; NAME -- the Dock's, the menu bar's, the window's first title -- is the app bundle's, set when it is built
;;;; (kiln mac build --name=...); these change the window as the app runs.
;;;;
;;;;   (window-title "Notes")
;;;;   (window-chrome :standard)       a title bar with the title and the three buttons
;;;;   (window-chrome :hidden-title)   the bar and its buttons, no title
;;;;   (window-chrome :transparent)    the content runs under the bar; the buttons float over it
;;;;   (window-chrome :none)           as :transparent, without the buttons (the bar's strip still drags it)
;;;;   (window-chrome :full-bleed)     no title bar at all: the content to every edge, the corners rounded;
;;;;                                   Command-drag anywhere moves the window
;;;;   (window-size 1280 800)          the content's size, in points
;;;;   (window-min-size 640 400)
;;;;   (window-fullscreen t)
;;;;   (window-remember "desk")        the window opens where it was last time
;;;;
;;;;   (window-pointer-shape :ibeam)   the pointer over the window: :arrow :ibeam :hand :open-hand :closed-hand
;;;;                                   :crosshair :resize-left-right :resize-up-down :not-allowed
;;;;
;;;; THE MAC'S OWN CHROME (modus's host/macos/swift/MacChrome.swift):
;;;;   (glass-panel x y w h &key radius tint)   Liquid Glass (macOS 26; frosted before): a UI handle, and what is
;;;;                                   added to it (kiln-swift:ui-add) sits inside, placed from its top left
;;;;   (glass-button button)           a native button drawn as glass
;;;;   (menu "Game")                   a menu in the menu bar, before Window
;;;;   (menu-item menu "Spin" fn &key key)   an item: FN is called when it is chosen; KEY "s" is Command-S
;;;;   (menu-separator menu)   (menu-check item on)   (menu-enable item on)
;;;;   (status-item &key title symbol menu fn)   at the right of the menu bar: text, an SF Symbol, or both;
;;;;                                   it opens MENU when clicked, or calls FN
;;;;   (status-title item "120 fps")
;;;;
;;;; TRACKPAD GESTURES come with the window's own events (type 15, pseudo-syscall 1004), for the app's loop:
;;;;   (gesture-event e)               (values kind phase amount) for a type 15 event E: KIND :pinch (AMOUNT the
;;;;                                   change, 0.1 a tenth bigger), :rotate (degrees, counter-clockwise), :smart-zoom,
;;;;                                   :swipe (:left :right :up :down); PHASE :began :changed :ended
;;;;
;;;; MORE WINDOWS than the one the desk (or app) is in:
;;;;   (new-window "Map" 360 360)      a window, shown, centred: a UI handle -- KILN-SWIFT:UI-ADD puts views in it,
;;;;                                   placed from its top left; a GPU view made to fill (GPU-VIEW 0 0 0 0) fills it
;;;;   (window-show win)  (window-hide win)  (window-retitle win "Map: 3 cubes")
;;;;   (on-window-close win fn)        FN, of no arguments, when its close button is clicked (it is hidden, and can
;;;;                                   be shown again)
;;;;   (on-window-resize win fn)       FN gets its new width and height, in points
;;;;
;;;; THE APP'S OWN EVENTS AND THE DOCK:
;;;;   (on-open fn)                    FN gets each file's path or link opened with the app: Finder's Open With
;;;;                                   (kiln mac --opens=UTIS), `open', the app's URL scheme (--url-scheme=NAME)
;;;;   (on-drop fn)                    FN gets each file's path dropped on the window
;;;;   (on-app-state fn)               FN gets :active :inactive :hidden or :shown as the app becomes so
;;;;   (on-quit fn)                    FN is called when the app is about to quit; the app waits for it to return
;;;;                                   (five seconds at most), then quits
;;;;   (dock-badge "3")  (dock-badge nil)   (dock-bounce &optional critical)
;;;;
;;;; With :transparent and :none the bar covers the top of the content.  The window host reports how much as the top
;;;; of the safe area and sends a resize, so the desk (which lays itself out in the safe area) keeps clear of it.

(defpackage #:kiln-mac
  (:use #:cl)
  (:export #:window-title #:window-chrome #:window-size #:window-min-size #:window-fullscreen #:window-remember
           #:window-pointer-lock #:window-pointer-shape
           #:glass-panel #:glass-button #:menu #:menu-item #:menu-separator #:menu-check #:menu-enable
           #:status-item #:status-title
           #:on-open #:on-drop #:on-app-state #:on-quit #:dock-badge #:dock-bounce
           #:new-window #:window-show #:window-hide #:window-retitle #:on-window-close #:on-window-resize
           #:gesture-event))

(in-package #:kiln-mac)

(defun %text-call (name text)
  (let ((b (kiln-swift::%octets text)))
    (kiln-swift:swift-call name (kiln-swift::%addr b) (length b))))

(defun window-title (title)
  "Put TITLE in the window's title bar."
  (%text-call "window_title" title))

(defun window-chrome (style)
  "STYLE is :standard, :hidden-title, :transparent (the content under the bar), :none (no title and no buttons)
   or :full-bleed (no title bar at all: the content to every edge)."
  (kiln-swift:swift-call "window_chrome"
                         (ecase style (:standard 0) (:hidden-title 1) (:transparent 2) (:none 3) (:full-bleed 4))))

(defun window-size (w h)
  "The content's size, W x H points."
  (kiln-swift:swift-call "window_size" w h))

(defun window-min-size (w h)
  "The smallest the person can make the content, W x H points."
  (kiln-swift:swift-call "window_min_size" w h))

(defun window-fullscreen (on)
  "Into full screen (ON true) or out of it."
  (kiln-swift:swift-call "window_fullscreen" (if on 1 0)))

(defun window-pointer-lock (on)
  "A game's mouse-look: ON locks the pointer -- hidden, held still, its moves sent as motion events (type 13: DX in
   the low 20 bits, DY in the next 20, two's complement, points) rather than places; NIL lets it go.  The window
   losing the keyboard lets it go too."
  (kiln-swift:swift-call "window_pointer_lock" (if on 1 0)))

(defun window-pointer-shape (shape)
  "The pointer's SHAPE over the window: :ARROW :IBEAM (text) :HAND (a link, a button) :OPEN-HAND (draggable)
   :CLOSED-HAND (dragging) :CROSSHAIR :RESIZE-LEFT-RIGHT :RESIZE-UP-DOWN :NOT-ALLOWED."
  (kiln-swift:swift-call "window_pointer_shape"
                         (or (position shape '(:arrow :ibeam :hand :open-hand :closed-hand :crosshair
                                               :resize-left-right :resize-up-down :not-allowed))
                             0)))

(defun window-remember (name)
  "Keep the window's frame under NAME: where the person leaves it is where it opens next time."
  (%text-call "window_remember" name))

;;; ---- the Mac's own chrome: glass, menus, status items --------------------------------------------------------------

(defun %bytes (text) (kiln-swift::%octets (or text "")))

(defun glass-panel (x y w h &key (radius 16) tint)
  "A Liquid Glass panel at X Y, W x H points (frosted before macOS 26), RADIUS its corners', TINT #RRGGBBAA or NIL.
   A UI handle: what is added to it with KILN-SWIFT:UI-ADD sits inside, its x and y from the panel's top left."
  (kiln-swift:swift-call "glass_new" x y w h radius (or tint 0)))

(defun glass-button (button &optional (on t))
  "BUTTON (a KILN-SWIFT:UI-BUTTON) drawn as glass."
  (kiln-swift:swift-call "glass_button" button (if on 1 0)))

(defun menu (title &key (in-bar t))
  "A menu titled TITLE, in the menu bar before Window (or, IN-BAR NIL, alone: a status item's)."
  (let ((b (%bytes title)))
    (kiln-swift:swift-call "menu_new" (kiln-swift::%addr b) (length b) (if in-bar 1 0))))

(defun menu-item (menu title fn &key (key ""))
  "An item of MENU titled TITLE; FN, of no arguments, is called when it is chosen.  KEY \"s\" is Command-S."
  (let* ((b (%bytes title)) (k (%bytes key))
         (item (kiln-swift:swift-call "menu_item_new" menu (kiln-swift::%addr b) (length b)
                                      (kiln-swift::%addr k) (length k))))
    (kiln-swift:on-event :menu item (lambda (value h) (declare (ignore value h)) (funcall fn)))
    item))

(defun menu-separator (menu) (kiln-swift:swift-call "menu_separator" menu))

(defvar *menu-states* (make-hash-table) "An item's (checked enabled), as last set.")

(defun %menu-state (item checked enabled)
  (setf (gethash item *menu-states*) (list checked enabled))
  (kiln-swift:swift-call "menu_item_set" item (if checked 1 0) (if enabled 1 0)))

(defun menu-check (item on)
  "ITEM checked (ON) or not."
  (%menu-state item on (second (gethash item *menu-states* (list nil t)))))

(defun menu-enable (item on)
  "ITEM choosable (ON) or greyed."
  (%menu-state item (first (gethash item *menu-states* (list nil t))) on))

(defun status-item (&key title symbol menu fn)
  "An item at the right of the menu bar: TITLE text, SYMBOL an SF Symbol, or both.  Clicked, it opens MENU (a MENU
   made with :IN-BAR NIL) or calls FN."
  (let* ((b (%bytes title)) (s (%bytes symbol))
         (item (kiln-swift:swift-call "status_item_new" (kiln-swift::%addr b) (length b)
                                      (kiln-swift::%addr s) (length s))))
    (when menu (kiln-swift:swift-call "status_item_menu" item menu))
    (when fn (kiln-swift:on-event :status item (lambda (value h) (declare (ignore value h)) (funcall fn))))
    item))

(defun status-title (item title)
  (let ((b (%bytes title)))
    (kiln-swift:swift-call "status_item_title" item (kiln-swift::%addr b) (length b))))

;;; ---- the app's own events, and the Dock ---------------------------------------------------------------------------

(defun on-open (fn)
  "FN, of one argument, gets each file's path, or link, opened with the app."
  (kiln-swift:on-event :opened 0 (lambda (what h) (declare (ignore h)) (funcall fn what))))

(defun on-drop (fn)
  "FN, of one argument, gets each file's path dropped on the window."
  (kiln-swift:on-event :dropped 0 (lambda (what h) (declare (ignore h)) (funcall fn what))))

(defun on-app-state (fn)
  "FN, of one argument, gets :ACTIVE :INACTIVE :HIDDEN or :SHOWN as the app becomes so."
  (kiln-swift:on-event :appstate 0
                       (lambda (state h)
                         (declare (ignore h))
                         (funcall fn (cond ((equal state "active") :active) ((equal state "inactive") :inactive)
                                           ((equal state "hidden") :hidden) (t :shown))))))

(defun on-quit (fn)
  "FN, of no arguments, is called when the app is about to quit; the app quits when it returns (or five seconds on)."
  (kiln-swift:swift-call "mac_on_quit" 1)
  (kiln-swift:on-event :quit 0 (lambda (value h)
                                 (declare (ignore value h))
                                 (unwind-protect (ignore-errors (funcall fn))
                                   (kiln-swift:swift-call "mac_quit_reply")))))

(defun dock-badge (text)
  "TEXT on the Dock icon's badge; NIL clears it."
  (let ((b (%bytes (if text (princ-to-string text) ""))))
    (kiln-swift:swift-call "dock_badge" (kiln-swift::%addr b) (length b))))

(defun dock-bounce (&optional critical)
  "The Dock icon bounces once -- until the app is active, if CRITICAL."
  (kiln-swift:swift-call "dock_bounce" (if critical 1 0)))

;;; ---- more windows ---------------------------------------------------------------------------------------------------

(defun new-window (title width height)
  "A window titled TITLE, WIDTH x HEIGHT points, centred and shown: a UI handle that views are added to."
  (let ((b (%bytes title)))
    (kiln-swift:swift-call "window_new" (kiln-swift::%addr b) (length b) width height)))

(defun window-show (win) (kiln-swift:swift-call "window_show" win))
(defun window-hide (win) (kiln-swift:swift-call "window_hide" win))

(defun window-retitle (win title)
  (let ((b (%bytes title)))
    (kiln-swift:swift-call "window_retitle" win (kiln-swift::%addr b) (length b))))

(defun on-window-close (win fn)
  "FN, of no arguments, when WIN's close button is clicked; WIN is hidden, and WINDOW-SHOW brings it back."
  (kiln-swift:on-event :winclose win (lambda (value h) (declare (ignore value h)) (funcall fn))))

(defun on-window-resize (win fn)
  "FN gets WIN's new content width and height, in points, as it is resized."
  (kiln-swift:on-event :winresize win (lambda (wh h) (declare (ignore h)) (funcall fn (floor wh 65536) (mod wh 65536)))))

;;; ---- trackpad gestures ----------------------------------------------------------------------------------------------

(defun gesture-event (e)
  "A trackpad gesture (a type 15 window event): (values kind phase amount), KIND :PINCH :ROTATE :SMART-ZOOM or :SWIPE,
   PHASE :BEGAN :CHANGED or :ENDED, AMOUNT a pinch's change (0.1: a tenth bigger), a rotation's degrees
   (counter-clockwise), a swipe's direction (:LEFT :RIGHT :UP :DOWN), NIL for a smart zoom."
  (let* ((kind (ldb (byte 4 32) e)) (phase (ldb (byte 2 36) e))
         (raw (ldb (byte 32 0) e)) (n (if (logbitp 31 raw) (- raw (ash 1 32)) raw)))
    (values (case kind (0 :pinch) (1 :rotate) (2 :smart-zoom) (t :swipe))
            (case phase (0 :began) (1 :changed) (t :ended))
            (case kind
              (0 (/ n 1000.0)) (1 (/ n 1000.0)) (2 nil)
              (t (case n (1 :left) (2 :right) (3 :up) (t :down)))))))
