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
;;;; With :transparent and :none the bar covers the top of the content.  The window host reports how much as the top
;;;; of the safe area and sends a resize, so the desk (which lays itself out in the safe area) keeps clear of it.

(defpackage #:kiln-mac
  (:use #:cl)
  (:export #:window-title #:window-chrome #:window-size #:window-min-size #:window-fullscreen #:window-remember
           #:window-pointer-lock
           #:glass-panel #:glass-button #:menu #:menu-item #:menu-separator #:menu-check #:menu-enable
           #:status-item #:status-title))

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
