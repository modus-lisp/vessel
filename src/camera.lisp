;;;; camera.lisp -- the camera, from Lisp: a live preview, the frames themselves, a photo, a video.  The Swift side
;;;; is modus's host/ios/swift/Camera.swift (cam_*), the same on the phone and on the Mac, called through
;;;; ios-swift.lisp's SWIFT-CALL.
;;;;
;;;;   (camera-names)                    -> ("FaceTime HD Camera" ...)
;;;;   (camera-open &optional index)     -> cam; then a :camera event, value 1 (allowed) or 0 -- the first open asks
;;;;   (camera-preview cam x y w h)      -> a UI handle: the camera, live, in the window
;;;;   (camera-frame cam)                -> the latest frame: a byte vector of BGRA rows, its width, its height
;;;;                                        (three values); NIL when none has come since the last one read
;;;;   (camera-photo cam path)           a JPEG at PATH; then :photo, value the path ("" if it failed)
;;;;   (camera-record cam path)          video (with sound, if the microphone may be used) to PATH, a .mov; then
;;;;                                        :recording, value the path, once it has started (a second or so later)
;;;;   (camera-stop cam)                 the recording ends; then :recorded, value the path ("" if it failed)
;;;;   (camera-close cam)
;;;;   (camera-folder)                   the app's own folder, where a photo or a video can go

(defpackage #:kiln-camera
  (:use #:cl)
  (:export #:camera-names #:camera-open #:camera-preview #:camera-frame #:camera-photo #:camera-record
           #:camera-stop #:camera-close #:camera-folder))

(in-package #:kiln-camera)

(defun %read-text (name &optional (cap 4096))
  "The UTF-8 text the Swift function NAME writes into a buffer, as a string (\"\" for none)."
  (let* ((buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (kiln-swift:swift-call name (kiln-swift::%addr buf) cap)))
    (cond ((zerop n) "")
          ((> n cap) (%read-text name n))
          (t (seal:utf8-decode buf :end n)))))

(defun %text-call (name cam text)
  (let ((b (kiln-swift::%octets text)))
    (kiln-swift:swift-call name cam (kiln-swift::%addr b) (length b))))

(defun camera-names ()
  "The cameras there are, by name, in the order CAMERA-OPEN numbers them."
  (let ((text (%read-text "cam_devices")))
    (when (plusp (length text))
      (loop with start = 0
            for end = (position #\Newline text :start start)
            collect (subseq text start end)
            while end
            do (setf start (1+ end))))))

(defun camera-open (&optional (index 0))
  "Camera INDEX (0, the first), a handle; NIL if there is no such camera.  A :camera event says whether it may be used."
  (let ((cam (kiln-swift:swift-call "cam_open" index)))
    (if (zerop cam) nil cam)))

(defun camera-preview (cam x y w h)
  "A live preview of CAM in the window, at X Y, W x H points: a UI handle."
  (kiln-swift:swift-call "cam_preview_new" cam x y w h))

(defvar *frame* nil "The buffer the frames are read into, kept and reused: a frame is megabytes.")

(defun camera-frame (cam)
  "The latest frame: (values bytes width height), the bytes BGRA rows (reused by the next call: copy to keep); NIL
   when no frame has come since the last one read."
  (let* ((size (kiln-swift:swift-call "cam_frame_size" cam))
         (w (logand size #xFFFF))
         (h (ash size -16))
         (need (* w h 4)))
    (when (plusp need)
      (unless (and *frame* (>= (length *frame*) need))
        (setf *frame* (make-array need :element-type '(unsigned-byte 8) :initial-element 0)))
      (let ((n (kiln-swift:swift-call "cam_frame" cam (kiln-swift::%addr *frame*) (length *frame*))))
        ;; 0: nothing new; more than the buffer: the camera changed size between the two calls (next time)
        (when (and (plusp n) (= n need))
          (values *frame* w h))))))

(defun camera-photo (cam path)
  "A still from CAM, as a JPEG at PATH; a :photo event follows, its value the path, or \"\" if it failed."
  (%text-call "cam_photo" cam path))

(defun camera-record (cam path)
  "Start recording video from CAM to PATH (a .mov), with sound if the microphone may be used.  A :recording event
   follows when it has started: the first second or so is not in it."
  (%text-call "cam_record" cam path))

(defun camera-stop (cam)
  "Stop recording; a :recorded event follows, its value the path, or \"\" if it failed."
  (kiln-swift:swift-call "cam_stop" cam))

(defun camera-close (cam)
  (kiln-swift:swift-call "cam_close" cam))

(defun camera-folder ()
  "The app's own folder (the phone's Documents; on a Mac, its folder in Application Support)."
  (%read-text "files_documents"))
