;;;; t420-usb-audio.lisp -- a USB Audio Class 1 device on the T420's EHCI controller.
;;;;
;;;; The first isochronous transfers on this controller.  A full- or low-speed device behind a
;;;; high-speed hub is reached by SPLIT transactions; for isochronous ones EHCI has its own
;;;; descriptor, the siTD (EHCI 1.0 section 3.4): one per frame, hung in the periodic frame list
;;;; in front of the interrupt QHs (the order the spec requires: isochronous first).  An IN siTD
;;;; starts its split in microframe 0 and collects the data with complete-splits in 2..5; an OUT
;;;; one carries up to 188 bytes in its start-split.
;;;;
;;;; What runs here is a burst, not a stream: every frame of the 1024-entry list gets an siTD
;;;; with its own buffer, the controller walks them once (1.024 s), and the list is put back.
;;;; That is enough to hear the microphone and to play a tone; a continuous stream needs the
;;;; ring refilled behind the frame index, which is the next step.
;;;;
;;;; The device found first (2026-11-24): KM_B2 Audio, 3302:2908, full speed on the Anker hub's
;;;; port 1 -- interface 1 a 16-bit stereo speaker at 44.1 or 48 kHz (isochronous OUT endpoint
;;;; 3, adaptive, 384 bytes), interface 2 a 16-bit mono microphone at 48 kHz (isochronous IN
;;;; endpoint #x83, asynchronous, 208 bytes), interface 3 HID (its buttons).
;;;;
;;;; MEMORY: the USB quarter of the net block, after t420-usb.lisp's 64 KB: the siTDs at
;;;; #x0F320000 (1024, 64 bytes apart), their buffers at #x0F330000 (1024 x 256 bytes).
;;;;
;;;; THE T420's EHCI IS 64-BIT CAPABLE (HCCPARAMS bit 0), and then an siTD is NINE dwords: words
;;;; 7 and 8 are its two buffer pointers' high halves (EHCI appendix B), as a qTD's high halves are
;;;; (t420-usb.lisp writes those).  Packed 32 bytes apart, word 8 was the next siTD's link pointer:
;;;; the controller read above 4 GB, stopped with Host System Error -- and with the BIOS's SMI on
;;;; host system error still enabled (it drove the boot disk then), the machine froze in SMM.

(defun %t4ua-sitd (f) (%t4u-va (+ #x20000 (* f 64))))   ; 36 bytes in 64-bit mode (below): 64 apart
(defun %t4ua-buf (f) (%t4u-va (+ #x30000 (* f 256))))

;;; ---- the device ----------------------------------------------------------------------------------

(defun t4-audio-describe (dev)
  "DEV's audio streaming interfaces, from its configuration descriptor: a list of plists
   (:iface :alt :ep :dir (:in or :out) :mps :channels :bytes :bits :rates)."
  (let ((n (%t4u-ctl dev #x80 6 #x200 0 255)) (b (%t4u-data-buf)) (out nil) (cur nil) (i 0))
    (when (plusp n)
      (loop while (< (+ i 1) n)
            do (let ((len (%t4u-byte b i)) (type (%t4u-byte b (+ i 1))))
                 (when (zerop len) (return))
                 (case type
                   (4 (setq cur (and (= (%t4u-byte b (+ i 5)) 1) (= (%t4u-byte b (+ i 6)) 2)   ; audio streaming
                                     (list :iface (%t4u-byte b (+ i 2)) :alt (%t4u-byte b (+ i 3))))))
                   (#x24 (when (and cur (= (%t4u-byte b (+ i 2)) 2))                          ; FORMAT_TYPE
                           (setf (getf cur :channels) (%t4u-byte b (+ i 4))
                                 (getf cur :bytes) (%t4u-byte b (+ i 5))
                                 (getf cur :bits) (%t4u-byte b (+ i 6))
                                 (getf cur :rates)
                                 (loop for k below (%t4u-byte b (+ i 7))
                                       for o = (+ i 8 (* 3 k))
                                       collect (logior (%t4u-byte b o) (ash (%t4u-byte b (+ o 1)) 8)
                                                       (ash (%t4u-byte b (+ o 2)) 16))))))
                   (5 (when (and cur (= 1 (logand (%t4u-byte b (+ i 3)) 3)))                  ; isochronous
                        (let ((a (%t4u-byte b (+ i 2))))
                          (setf (getf cur :ep) (logand a 15) (getf cur :dir) (if (logbitp 7 a) :in :out)
                                (getf cur :mps) (logand (%t4u-u16 b (+ i 4)) #x7FF))
                          (push cur out)
                          (setq cur nil)))))
                 (incf i len))))
    (nreverse out)))

(defun t4-audio-device ()
  "The first device on the bus with an audio streaming interface, or NIL."
  (find-if (lambda (d) (and (/= (getf d :class) 9) (ignore-errors (t4-audio-describe d))))
           *t4-usb-devices*))

(defun %t4ua-open (dev as rate)
  "Configure DEV, select AS's alternate setting and its sampling rate."
  (%t4u-ctl dev #x00 9 1 0 0)                                   ; SET_CONFIGURATION 1
  (%t4u-ctl dev #x01 11 (getf as :alt) (getf as :iface) 0)       ; SET_INTERFACE
  (let ((epaddr (logior (getf as :ep) (if (eq (getf as :dir) :in) #x80 0))))
    (t4-usb-control (getf dev :addr) (getf dev :speed) (getf dev :tthub) (getf dev :ttport)
                    (getf dev :mps0) #x22 1 #x100 epaddr 3                ; SET_CUR sampling frequency
                    (vector (logand rate #xFF) (logand (ash rate -8) #xFF) (ash rate -16)))))

(defun %t4ua-close (dev as)
  (%t4u-ctl dev #x01 11 0 (getf as :iface) 0))                   ; alternate setting 0: no bandwidth

;;; ---- one pass of the frame list ----------------------------------------------------------------------

(defun %t4ua-run (dev as bytes-for-frame &optional (frames 1024))
  "Hang an siTD on every frame for AS's endpoint, BYTES-FOR-FRAME bytes each (an OUT pass's
   buffers already hold their data), let the controller walk them once, take them out again.
   A list (frames-done bytes-moved errors missed)."
  (let* ((in (eq (getf as :dir) :in))
         (chars (logior (if in (ash 1 31) 0) (ash (getf dev :ttport) 24) (ash (getf dev :tthub) 16)
                        (ash (getf as :ep) 8) (getf dev :addr)))
         (fl (%t4u-frame-list))
         (old (%t4u-r fl)))                                      ; what every entry pointed at
    (dotimes (f 1024)
      (let* ((s (%t4ua-sitd f)) (pa (%t4u-pa (%t4ua-buf f))) (n (funcall bytes-for-frame f)))
        (%t4u-w s old)                                           ; next: the interrupt QHs
        (%t4u-w (+ s 4) chars)
        (%t4u-w (+ s 8) (if in (logior (ash #x3C 8) #x01) #x01))  ; IN: S-split 0, C-split 2..5
        (%t4u-w (+ s 16) pa)
        (%t4u-w (+ s 20) (logior (logand (+ pa 4096) #xFFFFF000) (if in 0 1)))   ; OUT: TP all, 1 split
        (%t4u-w (+ s 24) 1)                                      ; no back pointer
        (%t4u-w (+ s 28) 0) (%t4u-w (+ s 32) 0)                  ; the buffer pointers' high halves
        (%t4u-w (+ s 12) (logior (ash n 16) #x80))))             ; active, last
    (dotimes (f frames) (%t4u-w (+ fl (* 4 f)) (logior (%t4u-pa (%t4ua-sitd f)) 4)))
    (%t4u-sleep 1100)
    (dotimes (f 1024) (%t4u-w (+ fl (* 4 f)) old))
    (%t4u-sleep 3)                                               ; the controller lets go
    (let ((done 0) (moved 0) (errors 0) (missed 0))
      (dotimes (f frames)
        (let ((st (%t4u-r (+ (%t4ua-sitd f) 12))))
          (cond ((logbitp 7 st) (incf missed))
                ((plusp (logand st #x7C)) (incf errors))   ; bit 1 is the split state, not an error
                (t (incf done)
                   (incf moved (- (funcall bytes-for-frame f) (logand (ash st -16) #x3FF)))))))
      (list done moved errors missed))))

;;; ---- the microphone and the speaker --------------------------------------------------------------

(defun t4-audio-capture (&optional (dev (t4-audio-device)))
  "Record ~1 s from DEV's microphone.  A plist: frames, bytes, samples, min, max, rms, how many
   samples are 0, and the first few."
  (let ((as (find :in (t4-audio-describe dev) :key (lambda (a) (getf a :dir)))))
    (unless as (error "t420 audio: no microphone on ~A" (getf dev :name)))
    (%t4ua-open dev as (reduce #'max (getf as :rates)))
    (dotimes (f 1024) (%t4u-clear (%t4ua-buf f) 256))
    (let* ((mps (min 256 (getf as :mps)))
           (r (%t4ua-run dev as (lambda (f) (declare (ignore f)) mps)))
           (lo 32767) (hi -32768) (sum 0) (n 0) (zeros 0) (first-few nil))
      (%t4ua-close dev as)
      ;; each frame's bytes: what its siTD did not leave over
      (dotimes (f 1024)
        (let* ((st (%t4u-r (+ (%t4ua-sitd f) 12)))
               (got (if (logbitp 7 st) 0 (- mps (logand (ash st -16) #x3FF)))))
          (loop for i from 0 below (- got 1) by 2
                do (let* ((u (%t4u-u16 (%t4ua-buf f) i)) (v (if (> u 32767) (- u 65536) u)))
                     (when (< (length first-few) 8) (push v first-few))
                     (setq lo (min lo v) hi (max hi v) sum (+ sum (* v v)) n (+ n 1))
                     (when (zerop v) (incf zeros))))))
      (list :frames (first r) :bytes (second r) :errors (third r) :missed (fourth r)
            :samples n :min lo :max hi :rms (if (plusp n) (round (sqrt (/ sum n))) 0)
            :zeros zeros :first (nreverse first-few)))))

(defun t4-audio-tone (&key (hz 440) (level 6000) (dev (t4-audio-device)))
  "Play ~1 s of a sine at HZ through DEV's speaker (44.1 kHz stereo: 176 or 180 bytes a frame,
   one start-split each)."
  (let ((as (find :out (t4-audio-describe dev) :key (lambda (a) (getf a :dir)))))
    (unless as (error "t420 audio: no speaker on ~A" (getf dev :name)))
    (%t4ua-open dev as 44100)
    (let ((k 0) (sizes (make-array 1024)))
      (dotimes (f 1024)
        (let* ((frames (if (= 9 (mod f 10)) 45 44)) (buf (%t4ua-buf f)))
          (setf (aref sizes f) (* 4 frames))
          (dotimes (j frames)
            (let* ((v (round (* level (sin (/ (* 2 pi hz k) 44100))))) (u (logand v #xFFFF)))
              (dotimes (c 2)
                (setf (mem-ref (+ buf (* 4 j) (* 2 c)) :u8) (logand u #xFF)
                      (mem-ref (+ buf (* 4 j) (* 2 c) 1) :u8) (ash u -8))))
            (incf k))))
      (prog1 (%t4ua-run dev as (lambda (f) (aref sizes f)))
        (%t4ua-close dev as)))))
