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
;;;;   (follow-player p)     a warp-media player (the desk's Media) shown as Now Playing, and driven by the keys

(defpackage #:kiln-media
  (:use #:cl)
  (:export #:now-playing #:now-playing-clear #:on-media #:follow-player))

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

;;; ---- a warp-media player as Now Playing -----------------------------------------------------------------------
;;;
;;; FOLLOW-PLAYER shows the desk's player (warp's, under glass's Media) as what is playing and lets the media keys
;;; drive it.  Nothing is claimed until it first plays: on the phone, taking the commands makes the audio session
;;; the unmixed playback kind, which stops another app's sound -- right when Media plays, wrong when the desk opens.

(defvar *followed* nil "The warp-media player shown as Now Playing.")
(defvar *claimed* nil "True once the commands are taken (the player has played).")
(defvar *shown* nil "What was last told to the system: (title playing duration), the position, and when.")
(defvar *follow-tick* 0)

(defun %wm (name &rest args)
  (apply (symbol-function (find-symbol name "WARP-MEDIA")) args))

(defun follow-player (player)
  "Show warp-media's PLAYER as Now Playing, and drive it with the media keys, from when it first plays."
  (setf *followed* player *shown* nil)
  (pushnew '%follow-tick cl-user::*kiln-frame-hooks*)
  player)

(defun %player-command (c)
  (let ((p *followed*))
    (when p
      (case (if (consp c) (car c) c)
        (:play (if (eq (%wm "PLAYER-STATE" p) :paused) (%wm "RESUME" p) (%wm "TOGGLE" p)))
        ((:pause :stop) (%wm "PAUSE" p))        ; STOP would forget the track: the system's stop is a pause
        (:toggle (%wm "TOGGLE" p))
        (:next (%wm "PLAY-NEXT" p))
        (:previous (%wm "PLAY-PREV" p))
        (:seek (%wm "SEEK" p (second c))))
      (%follow-tick t))))

(defun %artwork (track)
  "A cover beside TRACK (cover, folder or front, .jpg or .png), or NIL."
  (when track
    (let ((dir (directory-namestring track)))
      (dolist (name '("cover.jpg" "folder.jpg" "front.jpg" "cover.png" "folder.png" "front.png"))
        (let ((f (concatenate 'string dir name)))
          (when (probe-file f) (return f)))))))

(defun %album (track)
  "TRACK's folder's name: what a music folder is named for."
  (when track
    (let* ((dir (string-right-trim "/" (directory-namestring track)))
           (slash (position #\/ dir :from-end t)))
      (if slash (subseq dir (1+ slash)) dir))))

(defun %follow-tick (&optional now)
  "A frame hook: twice a second (or NOW), what the player is doing, told to the system when it changes -- another
   track, playing or paused, or a jump the system's own clock would not have made (a seek)."
  (let ((p *followed*))
    (when (and p (or now (zerop (mod (incf *follow-tick*) 30))))
      (let* ((state (%wm "PLAYER-STATE" p))
             (title (%wm "PLAYER-TITLE" p))
             (playing (eq state :playing))
             (position (or (ignore-errors (%wm "PLAYER-POSITION" p)) 0))
             (duration (or (%wm "PLAYER-DURATION" p) 0))
             (key (list title playing duration)))
        (cond ((member state '(:loading)))           ; between tracks: keep what is shown
              ((or (null title) (member state '(:stopped :error)))
               (when *shown* (now-playing-clear) (setf *shown* nil)))
              ((or (not (equal key (first *shown*)))
                   (> (abs (- position (%expected-position))) 2))
               (unless *claimed*
                 (when playing (on-media #'%player-command) (setf *claimed* t)))
               (when *claimed*
                 (let ((track (%wm "PLAYER-TRACK" p)))
                   (now-playing :title title :album (%album track) :artwork (%artwork track)
                                :duration duration :position position :playing playing))
                 (setf *shown* (list key position (get-internal-real-time))))))))))

(defun %expected-position ()
  "Where the system has the track now: where it was told, moved on by the time since if playing."
  (if (null *shown*)
      0
      (destructuring-bind ((title playing duration) at when) *shown*
        (declare (ignore title duration))
        (if playing (+ at (/ (- (get-internal-real-time) when) internal-time-units-per-second)) at))))

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
