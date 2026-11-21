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
;;;; With :transparent and :none the bar covers the top of the content.  The window host reports how much as the top
;;;; of the safe area and sends a resize, so the desk (which lays itself out in the safe area) keeps clear of it.

(defpackage #:kiln-mac
  (:use #:cl)
  (:export #:window-title #:window-chrome #:window-size #:window-min-size #:window-fullscreen #:window-remember
           #:window-pointer-lock))

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
