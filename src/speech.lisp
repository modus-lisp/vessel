;;;; speech.lisp -- the phone's voice and ear: the platform's speech (AVSpeechSynthesizer, SFSpeechRecognizer,
;;;; modus's host/ios/swift/Speech.swift) behind glass's voice protocol, so the desk's Speak and Listen apps
;;;; (glass/voice-apps) run on the phone as they run on a desktop over chord and stave.
;;;;
;;;;   (install-platform-speech)       make the platform's voice and ear glass's *VOICE* and *EAR*, when the
;;;;                                   bridge has them; NIL where it does not (a watch, a desktop)
;;;;   (say-to-file "text" path)       say TEXT into a WAV at PATH, not aloud; a handle for FILE-SAID
;;;;   (hear-file path)                transcribe the audio file at PATH into the ear's transcript
;;;;   (round-trip "text" (lambda (heard) ...))   say TEXT to a file, transcribe it, and call back with what
;;;;                                   was heard -- from the frame hooks, so nothing waits

(defpackage #:kiln-speech
  (:use #:cl)
  (:export #:platform-voice #:platform-ear #:install-platform-speech #:say-to-file #:file-said #:hear-file
           #:ear-busy-p #:round-trip #:normalize))

(in-package #:kiln-speech)

(defun %call (name &rest args) (apply #'kiln-swift:swift-call name args))

(defun %text-call (name &rest args)
  "A call that answers with text: NAME's bytes, decoded."
  (let* ((cap 4096)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (apply #'%call name (append args (list (kiln-swift::%addr buf) cap)))))
    (if (or (zerop n) (> n cap)) "" (seal:utf8-decode buf :end n))))

(defun %bytes (text) (kiln-swift::%octets text))

;;; ---- the voice ---------------------------------------------------------------------------------------

(defclass platform-voice () ()
  (:documentation "The platform's voice: AVSpeechSynthesizer, through the speaker."))

(defmethod glass.desk:voice-say ((v platform-voice) text)
  (let ((b (%bytes text))) (%call "speech_say" (kiln-swift::%addr b) (length b))))

(defmethod glass.desk:voice-hush ((v platform-voice)) (%call "speech_hush"))

(defmethod glass.desk:voice-state ((v platform-voice))
  (if (= (%call "speech_speaking") 1) "Speaking..." "Ready"))

(defmethod glass.desk:voice-report ((v platform-voice)) "the phone's own voice")

;;; ---- the ear -----------------------------------------------------------------------------------------

(defclass platform-ear () ()
  (:documentation "The platform's ear: SFSpeechRecognizer on the microphone, or on a file."))

(defun %ear-state () (%call "ear_state"))

(defmethod glass.desk:ear-start ((e platform-ear)) (%call "ear_start"))
(defmethod glass.desk:ear-stop ((e platform-ear)) (%call "ear_stop"))
(defmethod glass.desk:ear-clear ((e platform-ear)) (%call "ear_clear"))

(defmethod glass.desk:ear-state ((e platform-ear))
  (case (%ear-state)
    (0 "Idle")
    (1 "Asking for permission...")
    (2 (if (plusp (length (%text-call "ear_partial"))) "Hearing..." "Listening"))
    (3 "Not allowed: Settings, modus, Microphone and Speech Recognition")
    (4 "The microphone did not start")
    (5 "Transcribing a file...")
    (t "?")))

(defmethod glass.desk:ear-report ((e platform-ear)) "the phone's own recognizer")
(defmethod glass.desk:ear-text ((e platform-ear)) (%text-call "ear_text"))
(defmethod glass.desk:ear-level ((e platform-ear)) (/ (%call "ear_level") 1d6))

(defun ear-busy-p ()
  "T while the ear is asking for permission or transcribing a file."
  (member (%ear-state) '(1 5)))

;;; ---- installing them, and files -----------------------------------------------------------------------

(defun install-platform-speech ()
  "Make the platform's voice and ear glass's *VOICE* and *EAR*, when the bridge has them."
  (when (ignore-errors (%call "speech_speaking") t)
    (setf glass.desk:*voice* (make-instance 'platform-voice)
          glass.desk:*ear* (make-instance 'platform-ear))
    t))

(defun say-to-file (text path)
  "Say TEXT into a WAV at PATH, not aloud.  A handle: FILE-SAID answers 0 writing, 1 written, -1 failed."
  (let ((b (%bytes text)) (p (%bytes path)))
    (%call "speech_to_file" (kiln-swift::%addr b) (length b) (kiln-swift::%addr p) (length p))))

(defun file-said (handle)
  (let ((r (%call "speech_file_done" handle)))
    (if (>= r (ash 1 63)) (- r (ash 1 64)) r)))       ; the word is unsigned; -1 comes back as 2^64-1

(defun hear-file (path)
  (let ((p (%bytes path))) (%call "ear_file" (kiln-swift::%addr p) (length p))))

(defun normalize (text)
  "TEXT lowercased, with only letters, digits and single spaces: what a recognizer heard, comparable with
   what was said."
  (let ((out (make-string-output-stream)) (space nil))
    (loop for ch across (string-downcase text)
          do (cond ((alphanumericp ch) (when space (write-char #\Space out)) (setf space nil) (write-char ch out))
                   (t (setf space (plusp (file-position out))))))
    (get-output-stream-string out)))

(defun %temp-path (name)
  "NAME in the app's temporary directory ($TMPDIR, which may or may not end in a slash)."
  (let ((dir (or (modus-int::%cli-getenv "TMPDIR") "/tmp")))
    (concatenate 'string (string-right-trim "/" dir) "/" name)))

(defun round-trip (text done &key (path (%temp-path "kiln-round-trip.caf")) said)
  "Say TEXT into a file, transcribe the file, and call DONE with what was heard (\"\" if nothing, and :FAILED
   when the recognizer could not run) -- driven from the frame hooks, a step a frame, so the app never waits on
   it.  SAID, when given, is called with T or NIL as soon as the file is written or has failed."
  (glass.desk:ear-clear glass.desk:*ear*)
  (let ((handle (say-to-file text path)) (stage :saying) (hook nil) (frames 0))
    (setf hook
          (lambda ()
            (incf frames)
            (case stage
              (:saying
               (case (file-said handle)
                 (1 (when said (funcall said t)) (hear-file path) (setf stage :hearing frames 0))
                 (-1 (when said (funcall said nil)) (setf stage :done) (funcall done ""))))
              (:hearing
               ;; asking (1) or transcribing (5) still; give it time to start before believing idle
               (when (and (> frames 30) (not (ear-busy-p)))
                 (setf stage :done)
                 (funcall done (if (= (%ear-state) 4) :failed (glass.desk:ear-text glass.desk:*ear*))))))
            (when (eq stage :done)
              (setf cl-user::*kiln-frame-hooks* (remove hook cl-user::*kiln-frame-hooks*)))))
    (push hook cl-user::*kiln-frame-hooks*)
    handle))
