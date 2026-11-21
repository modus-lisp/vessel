;;;; now-playing.lisp -- the system's Now Playing and the media keys, from Lisp: say what is playing, and hear the
;;;; play/pause, next and previous keys (a Mac's keyboard, headphone buttons, Control Center, the lock screen).  The
;;;; Swift side is modus's host/ios/swift/NowPlaying.swift (np_*), the same on the phone and on the Mac.
;;;;
;;;;   (now-playing &key title artist album duration position playing artwork)
;;;;        shown as what is playing: DURATION and POSITION in seconds, PLAYING true or false (the system moves the
;;;;        position on while it is), ARTWORK an image file's path.  Call again when anything changes.
;;;;   (now-playing-clear)   nothing is playing
;;;;   (on-media fn)         FN is called with a command: :play :pause :toggle :stop :next :previous, or
;;;;                         (:seek seconds).  One FN at a time: the app that plays.

(defpackage #:kiln-media
  (:use #:cl)
  (:export #:now-playing #:now-playing-clear #:on-media))

(in-package #:kiln-media)

(defvar *handle* nil "The handle the media commands' events come from: taking them is asked for once.")

(defun %handle ()
  (or *handle* (setf *handle* (kiln-swift:swift-call "np_enable"))))

(defun %text (field text)
  (let ((b (kiln-swift::%octets (or text ""))))
    (kiln-swift:swift-call "np_text" field (kiln-swift::%addr b) (length b))))

(defun now-playing (&key title artist album (duration 0) (position 0) playing artwork)
  "Show TITLE, ARTIST and ALBUM as what is playing, DURATION seconds long, at POSITION, PLAYING or paused; ARTWORK
   an image file's path."
  (%handle)
  (%text 0 title)
  (%text 1 artist)
  (%text 2 album)
  (let ((b (kiln-swift::%octets (or artwork ""))))
    (kiln-swift:swift-call "np_artwork" (kiln-swift::%addr b) (length b)))
  (kiln-swift:swift-call "np_times" (max 0 (round (* 1000 duration))) (max 0 (round (* 1000 position)))
                         (if playing 1 0))
  t)

(defun now-playing-clear ()
  (kiln-swift:swift-call "np_clear")
  t)

(defun on-media (fn)
  "Call FN with each media command: :play :pause :toggle :stop :next :previous, or (:seek seconds)."
  (let ((h (%handle)))
    (kiln-swift:on-event :media h
                         (lambda (value h)
                           (declare (ignore h))
                           (funcall fn (cond ((equal value "play") :play) ((equal value "pause") :pause)
                                             ((equal value "toggle") :toggle) ((equal value "stop") :stop)
                                             ((equal value "next") :next) ((equal value "previous") :previous)
                                             (t value)))))
    (kiln-swift:on-event :seek h
                         (lambda (value h)
                           (declare (ignore h))
                           (funcall fn (list :seek (/ value 1000.0)))))
    t))
