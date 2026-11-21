;;;; sound.lisp -- a game's sounds, from Lisp: sounds made (or loaded) once, played as voices -- each with its own
;;;; gain and pan, changed every frame for a sound that moves -- mixed here into stereo and written to the speaker
;;;; (pseudo-syscalls 1010-1012, modus's host/macos/modus-audio.c, opened with two channels).  No glass: a game has
;;;; its own loop, and calls SOUND-PUMP once a frame to keep the speaker fed.
;;;;
;;;;   (sound-open)                         the speaker, stereo, 48 kHz; NIL if there is none
;;;;   (tone frequency &key seconds timbre) a sound: a steady tone (whole cycles, so it loops without a click)
;;;;   (pluck frequency &key seconds)       a sound: a note that dies away
;;;;   (play sound &key gain pan loop)      a voice, playing; PAN -1 left .. 1 right
;;;;   (voice-set voice &key gain pan)      a voice's new gain and pan (a moving thing): ramped over the next mix,
;;;;                                        so a step is not a click
;;;;   (voice-stop voice)
;;;;   (sound-pump)                         once a frame: mix and write until the speaker holds *CUSHION* ahead
;;;;   (place listener-x listener-z yaw source-x source-y source-z &key loudness)
;;;;                                        (values gain pan) for a source heard from a listener facing YAW
;;;;
;;;; SPATIAL: sound placed in three dimensions by Apple's binaural renderer (modus's host/ios/swift/Spatial.swift):
;;;; in front, behind, above, below -- and with AirPods, held still in the world as the head turns.  The samples
;;;; are still Lisp's (TONE, PLUCK); the placing and the rendering are the platform's.
;;;;   (spatial-open)                       T when the renderer is there
;;;;   (spatial-voice sound &key loop)      a voice, from a sound handed over once
;;;;   (spatial-place voice x y z &key gain)   where it is, each frame
;;;;   (spatial-listener x y z yaw pitch)   where the listener is and which way it faces (yaw 0 down -Z)
;;;;   (spatial-stop voice)   (spatial-volume 0.0..1.0)

(defpackage #:kiln-sound
  (:use #:cl)
  (:export #:sound-open #:tone #:pluck #:play #:voice-set #:voice-stop #:sound-pump #:place #:*rate* #:*cushion*
           #:*master*
           #:spatial-open #:spatial-voice #:spatial-place #:spatial-listener #:spatial-stop #:spatial-volume))

(in-package #:kiln-sound)

(defvar *rate* 48000)
(defparameter *cushion* 2400 "Frames kept queued ahead: 50 ms at 48 kHz, enough to ride out a slow frame.")
(defparameter *chunk* 480 "Frames mixed at a time: 10 ms.")
(defvar *master* 1.0 "Every voice's gain is scaled by this: 0 is silence.")
(defvar *open* nil)
(defvar *voices* '())
;; the mix, reused: two float lanes, and the interleaved 16-bit samples the speaker takes
(defvar *left* nil)
(defvar *right* nil)
(defvar *out* nil)

(defun %sys (n a b c) (cl-user::%kiln-sys n a b c))

(defun sound-open (&optional (rate 48000))
  "The speaker, stereo at RATE; T, or NIL when there is none."
  (setf *rate* rate
        *left* (make-array *chunk* :element-type 'single-float :initial-element 0.0)
        *right* (make-array *chunk* :element-type 'single-float :initial-element 0.0)
        ;; (SIGNED-BYTE 16) as modus stores it: a fixnum a word, as AUDIO-WRITE reads
        *out* (make-array (* 2 *chunk*) :element-type '(signed-byte 16) :initial-element 0)
        *open* (zerop (%sys 1010 rate 2 0))))

;;; ---- sounds -------------------------------------------------------------------------------------------------------

(defun %sound (frames fn)
  (let ((s (make-array frames :element-type 'single-float)))
    (dotimes (i frames s) (setf (aref s i) (float (funcall fn i) 1.0)))))

(defun tone (frequency &key (seconds 1) (timbre '(0.6 0.25 0.1)))
  "A steady tone at FREQUENCY (Hz, a whole number, so SECONDS hold whole cycles and it loops cleanly), its
   harmonics at the strengths in TIMBRE."
  (let ((w (/ (* 2 pi frequency) *rate*)))
    (%sound (* seconds *rate*)
            (lambda (i) (loop for a in timbre for k from 1 sum (* a (sin (* k w i))))))))

(defun pluck (frequency &key (seconds 0.25))
  "A note that starts bright and dies away: a blip, a click, a ping."
  (let ((w (/ (* 2 pi frequency) *rate*)) (n (round (* seconds *rate*))))
    (%sound n (lambda (i)
                (let ((decay (exp (/ (* -6.0 i) n))))
                  (* decay (+ (* 0.7 (sin (* w i))) (* 0.3 (sin (* 2 w i))))))))))

;;; ---- voices -------------------------------------------------------------------------------------------------------

(defstruct voice
  (samples nil) (at 0) (loop nil)
  ;; the gains each side mixes at now, and what they ramp to over the next mix
  (l 0.0) (r 0.0) (to-l 0.0) (to-r 0.0)
  (live t))

(defun %gains (gain pan)
  "Constant-power pan: GAIN at PAN (-1 left .. 1 right) as (values left right)."
  (let ((a (* (/ pi 4) (+ 1 (max -1.0 (min 1.0 pan))))))
    (values (float (* gain (cos a)) 1.0) (float (* gain (sin a)) 1.0))))

(defun play (sound &key (gain 1.0) (pan 0.0) loop)
  "SOUND, playing from its start: a voice."
  (multiple-value-bind (l r) (%gains gain pan)
    (let ((v (make-voice :samples sound :loop loop :l l :r r :to-l l :to-r r)))
      (push v *voices*)
      v)))

(defun voice-set (voice &key (gain 1.0) (pan 0.0))
  (multiple-value-bind (l r) (%gains gain pan)
    (setf (voice-to-l voice) l (voice-to-r voice) r)))

(defun voice-stop (voice) (setf (voice-live voice) nil))

;;; ---- the mix ------------------------------------------------------------------------------------------------------

(defun %mix-voice (v n)
  "V's next N frames into the mix, its gains ramped from where they were to where they are going."
  (declare (type fixnum n))
  (let* ((s (voice-samples v)) (len (length s)) (at (voice-at v))
         (l (voice-l v)) (r (voice-r v))
         (dl (/ (- (voice-to-l v) l) n)) (dr (/ (- (voice-to-r v) r) n))
         (left *left*) (right *right*))
    (declare (type (simple-array single-float (*)) s left right) (type fixnum len at)
             (type single-float l r dl dr))
    (dotimes (i n)
      (when (>= at len)
        (if (voice-loop v) (setf at 0) (progn (setf (voice-live v) nil) (return))))
      (let ((x (aref s at)))
        (incf (aref left i) (* x l))
        (incf (aref right i) (* x r)))
      (incf l dl) (incf r dr) (incf at))
    (setf (voice-at v) at (voice-l v) (voice-to-l v) (voice-r v) (voice-to-r v))))

(defun %mix (n)
  "N frames of every voice, into *OUT* as 16-bit stereo."
  (fill *left* 0.0) (fill *right* 0.0)
  (dolist (v *voices*) (when (voice-live v) (%mix-voice v n)))
  (setf *voices* (delete-if-not #'voice-live *voices*))
  (let ((out *out*) (m (* 32767.0 *master*)))
    (dotimes (i n)
      ;; a soft limit, so a crowd of voices bends rather than clips
      (flet ((s16 (x) (round (* m (/ x (+ 1.0 (abs x)))))))
        (setf (aref out (* 2 i)) (s16 (aref *left* i))
              (aref out (1+ (* 2 i))) (s16 (aref *right* i)))))))

(defun sound-pump ()
  "Once a frame: mix and write until the speaker holds *CUSHION* frames ahead."
  (when *open*
    (loop repeat 20
          while (< (floor (%sys 1012 0 0 0) 2) *cushion*)
          do (%mix *chunk*)
             (%sys 1011 (+ (modus-int::%gc-word-of *out* (modus-int::%conv-addr #x100050A0)) 7) (* 2 *chunk*) 0))))

;;; ---- where a sound is ---------------------------------------------------------------------------------------------

(defun place (lx lz yaw sx sy sz &key (loudness 1.0) (near 2.5) (ly 1.6))
  "A source at SX SY SZ heard from a listener at LX LY LZ facing YAW (0 down -Z, as hello3d's camera): (values gain
   pan).  Louder near (full within NEAR), fainter as the square of distance past it; panned by how far to the
   listener's right it lies."
  (let* ((dx (- sx lx)) (dy (- sy ly)) (dz (- sz lz))
         (d (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
         (gain (* loudness (/ 1.0 (+ 1.0 (expt (/ (max 0.0 (- d near)) near) 2)))))
         ;; the listener's right, level: (cos yaw, -sin yaw) in x z
         (pan (if (< d 1e-3) 0.0 (/ (+ (* dx (cos yaw)) (* dz (- (sin yaw)))) d))))
    (values (float gain 1.0) (float pan 1.0))))

;;; ---- spatial ------------------------------------------------------------------------------------------------------

(defvar *spatial* nil "True once the platform's 3D renderer is running.")
(defvar *spatial-sounds* (make-hash-table :test 'eq) "A sound's handle there, by the sound: handed over once.")
(defvar *args* (make-array 9 :element-type 'single-float :initial-element 0.0) "Floats for a call, reused.")

(defun %call (name &rest args) (apply #'kiln-swift:swift-call name args))
(defun %addr (v) (kiln-swift::%addr v))

(defun spatial-open ()
  "The platform's 3D sound renderer: T when it is there and running."
  (or *spatial*
      (setf *spatial* (and (find-package "KILN-SWIFT") (eql 1 (ignore-errors (%call "spatial_start")))))))

(defun %spatial-sound (sound)
  (or (gethash sound *spatial-sounds*)
      (setf (gethash sound *spatial-sounds*) (%call "spatial_sound" (%addr sound) (length sound) *rate*))))

(defun spatial-voice (sound &key loop)
  "SOUND (a TONE or PLUCK) playing in 3D, at the listener until placed: a voice, or NIL."
  (when (spatial-open)
    (let ((v (%call "spatial_voice" (%spatial-sound sound) (if loop 1 0))))
      (and (plusp v) v))))

(defun spatial-place (voice x y z &key (gain 1.0))
  (let ((a *args*))
    (setf (aref a 0) (float x 1.0) (aref a 1) (float y 1.0) (aref a 2) (float z 1.0) (aref a 3) (float gain 1.0))
    (%call "spatial_place" voice (%addr a))))

(defun spatial-listener (x y z yaw &optional (pitch 0.0))
  "The listener at X Y Z, facing YAW (about the vertical; 0 is down -Z, positive turns left) and PITCH (up)."
  (let* ((a *args*) (cp (cos pitch))
         (fx (* -1 (sin yaw) cp)) (fy (sin pitch)) (fz (* -1 (cos yaw) cp))
         ;; up: perpendicular to forward, in the plane of forward and the world's up
         (ux (* (sin yaw) (sin pitch))) (uy cp) (uz (* (cos yaw) (sin pitch))))
    (loop for v in (list x y z fx fy fz ux uy uz) for i from 0 do (setf (aref a i) (float v 1.0)))
    (%call "spatial_listener" (%addr a))))

(defun spatial-stop (voice) (%call "spatial_stop" voice))

(defun spatial-volume (v) (%call "spatial_volume" (round (* 1000 (max 0.0 (min 1.0 v))))))
