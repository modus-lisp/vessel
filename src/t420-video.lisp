;;;; t420-video.lisp -- hardware video decode on the T420's GPU (Sandy Bridge's MFX engine).
;;;;
;;;; Sandy Bridge's video engine decodes H.264, MPEG-2 and VC-1 in fixed function -- entropy
;;;; decoding, the inverse transform, motion compensation, the loop filter -- from a command stream
;;;; on its own ring (VCS, #x12000).  The CPU parses the headers and keeps the reference set; the
;;;; hardware does the rest.  The command sequence is the libva driver's for this generation
;;;; (intel-vaapi-driver src/gen6_mfd.c, MIT, Intel), with the H.264 parsing, order counts,
;;;; reference marking and lists taken from reel's software decoder (reel/src/h264): the hardware
;;;; path replaces only its macroblock layer and loop filter, so the two decoders agree on
;;;; everything a picture is decoded FROM, and reel's own output is the check.
;;;;
;;;; SURFACES are NV12, Y-tiled (the engine requires it): a luma plane of PITCH x H32 and the
;;;; interleaved chroma below it, at row H32.  The CPU reads one back through a FENCE register,
;;;; which makes the aperture present a tiled range as linear.

;;; ---- the video ring -----------------------------------------------------------------------------

(defvar *t4v-vcs* nil "The video engine's ring.")

(defun t4v-submit (r)
  "The video ring's tail move, as gen 6 needs it (Linux gen6_bsd_submit_request): idle messages
   off, the context id cleared, wait for the ring to wake, the tail, idle messages on."
  (when (logtest (t4r-tail r) 4) (t4g-emit r (list 0)))
  (t4-gpu-set #x12050 (logior (ash 1 16) 1))
  (t4-gpu-set #x12198 0) (t4-gpu-set #x1219C 0)
  (dotimes (i 100000) (unless (logbitp 3 (t4-gpu-reg #x12050)) (return)))
  (t4-gpu-set (+ (t4r-base r) #x30) (t4r-tail r))
  (t4-gpu-set #x12050 (ash 1 16)))

(defun t4v-start ()
  (or *t4v-vcs*
      (progn (t4g-forcewake) (setq *t4v-vcs* (t4g-ring-start #x12000 "video")))))

;;; ---- surfaces -------------------------------------------------------------------------------------

(defstruct (t4v-surface (:conc-name t4vs-)) gpu w h pitch h32 dmv)

(defvar *t4v-pool* '() "Surfaces no decoder holds: GPU memory and CPU views are never given back.")

(defun t4v-surface-new (mbw mbh)
  "An NV12 surface for a picture MBW x MBH macroblocks, and its direct-MV buffer: one from the
   pool if it has the size, else new."
  (let ((s (find-if (lambda (s) (and (= (t4vs-w s) (* 16 mbw)) (= (t4vs-h s) (* 16 mbh)))) *t4v-pool*)))
    (when s (setq *t4v-pool* (remove s *t4v-pool* :count 1)) (return-from t4v-surface-new s)))
  (let* ((w (* 16 mbw)) (h (* 16 mbh))
         (pitch (* 128 (ceiling w 128))) (h32 (* 32 (ceiling h 32)))
         (rows (+ h32 (* 32 (ceiling (ash h -1) 32)))))
    (make-t4v-surface :gpu (t4g-alloc (* pitch rows) :align 4096 :map nil)
                      :w w :h h :pitch pitch :h32 h32
                      :dmv (t4g-alloc (* 128 mbh 64) :align 4096 :map nil))))

;;; ---- reading a tiled surface back ----------------------------------------------------------------
;;; FENCE_REG_SANDYBRIDGE_0 (#x100000, 16 x 8 bytes): low dword the first page | Y tiling (bit 1) |
;;; valid; high dword the LAST page | (pitch / 128 - 1).  Through a fence the aperture reads as
;;; linear rows.  Fence 15 is this file's.

(defvar *t4v-fence-va* nil)

(defun %t4v-fence (gpu bytes pitch)
  (unless *t4v-fence-va*
    (setq *t4v-fence-va* (modus-int::pc-mmio-map
                          (+ (logand (modus-int::pci-config-read 0 2 0 #x10) (lognot 15)) #x100000) #x1000)))
  (let ((r (+ *t4v-fence-va* (* 8 15))))
    (t4g-w r 0)
    (t4g-w (+ r 4) (logior (logand (+ gpu bytes -4096) #xFFFFF000) (- (floor pitch 128) 1)))
    (t4g-w r (logior (logand gpu #xFFFFF000) 2 1))))

(defvar *t4v-staging* nil "(gpu . cpu): 256 KB the CPU reads surfaces through, mapped once.")
(defparameter +t4v-staging-bytes+ (* 256 1024))

(defun %t4v-row-reader (s)
  "A function (ROW X) -> the 32-bit word at byte X of surface row ROW (chroma rows follow the
   luma's H32).  The surface's tiled bytes come over in strips of whole tile rows (32 rows), by
   the blitter as a plain byte copy, into the staging buffer; the fence detiles them there.  So
   reading any surface costs no more of the MMIO window than the staging buffer."
  (unless *t4v-staging*
    (multiple-value-bind (g c) (t4g-alloc +t4v-staging-bytes+) (setq *t4v-staging* (cons g c))))
  (let* ((pitch (t4vs-pitch s)) (band (* pitch 32))
         (rows (+ (t4vs-h32 s) (* 32 (ceiling (ash (t4vs-h s) -1) 32))))
         (bands (max 1 (floor +t4v-staging-bytes+ band))) (loaded -1))
    (when (> band +t4v-staging-bytes+) (error "t420 video: a tile row wider than the staging buffer"))
    (%t4v-fence (car *t4v-staging*) (* bands band) pitch)
    (lambda (row x)
      (let ((strip (floor row (* 32 bands))))
        (unless (= strip loaded)
          (t4g-blt-copy (car *t4v-staging*) pitch 0 0 (+ (t4vs-gpu s) (* strip bands band)) pitch 0 0
                        (floor pitch 4) (min (* 32 bands) (- rows (* strip 32 bands))))
          (let ((seq (t4g-breadcrumb *t4g-blt*)))
            (t4g-submit *t4g-blt*)
            (unless (t4g-wait *t4g-blt* seq 500) (error "t420 video: the blitter did not finish a readback")))
          (setq loaded strip))
        (t4g-r (+ (cdr *t4v-staging*) (* (- row (* strip 32 bands)) pitch) x))))))

(defun t4v-read-planes (s width height)
  "The decoded picture in S as three octet vectors (Y U V), WIDTH x HEIGHT (cropped from the top
   left)."
  (let* ((rd (%t4v-row-reader s))
         (cw (ash width -1)) (ch (ash height -1))
         (y (make-array (* width height) :element-type '(unsigned-byte 8)))
         (u (make-array (* cw ch) :element-type '(unsigned-byte 8)))
         (v (make-array (* cw ch) :element-type '(unsigned-byte 8))))
    (dotimes (r height)
      (loop for x from 0 below width by 4
            do (let ((d (funcall rd r x)))
                 (loop for k from 0 below (min 4 (- width x))
                       do (setf (aref y (+ (* r width) x k)) (ldb (byte 8 (* 8 k)) d))))))
    (dotimes (r ch)
      (loop for x from 0 below width by 4
            do (let ((d (funcall rd (+ (t4vs-h32 s) r) x)) (c (ash x -1)))
                 (when (< c cw) (setf (aref u (+ (* r cw) c)) (ldb (byte 8 0) d)
                                      (aref v (+ (* r cw) c)) (ldb (byte 8 8) d)))
                 (when (< (1+ c) cw) (setf (aref u (+ (* r cw) c 1)) (ldb (byte 8 16) d)
                                           (aref v (+ (* r cw) c 1)) (ldb (byte 8 24) d))))))
    (list y u v)))

(defun t4v-fnv (bytes)
  "FNV-1a, 32 bits: kiln's piece hash, for comparing a plane with the host's."
  (let ((h 2166136261))
    (dotimes (i (length bytes) h)
      (setq h (logand (* (logxor h (aref bytes i)) 16777619) #xFFFFFFFF)))))

;;; ---- an Annex B stream, as NAL units with their raw bytes --------------------------------------

(defun t4v-split-annex-b (bytes)
  "((start . end) ...) of each NAL unit in BYTES (after its start code, before the next)."
  (let ((out nil) (n (length bytes)) (i 0) (start nil))
    (loop while (< (+ i 2) n)
          do (if (and (zerop (aref bytes i)) (zerop (aref bytes (1+ i))) (= 1 (aref bytes (+ i 2))))
                 (progn
                   (when start
                     (let ((e i)) (loop while (and (> e start) (zerop (aref bytes (1- e)))) do (decf e))
                       (push (cons start e) out)))
                   (setq start (+ i 3) i (+ i 3)))
                 (incf i)))
    (when start (push (cons start n) out))
    (nreverse out)))

(defun %t4v-raw-offset (bytes start rbsp-byte)
  "The raw byte (from the NAL header at START) where RBSP byte RBSP-BYTE of its payload is: the
   payload begins after the 1-byte header, and each emulation-prevention 03 after 00 00 is a raw
   byte the RBSP does not have."
  (let ((i (1+ start)) (k 0) (zeros 0))
    (loop while (< k rbsp-byte)
          do (let ((b (aref bytes i)))
               (if (and (>= zeros 2) (= b 3))
                   (setq zeros 0)                      ; skipped: not an RBSP byte
                   (progn (setq zeros (if (zerop b) (1+ zeros) 0)) (incf k))))
             (incf i))
    (- i start)))

;;; ---- the H.264 decoder ---------------------------------------------------------------------------

(defstruct (t4v-dec (:conc-name t4vd-))
  (reel (reel.h264::make-decoder))
  (surfaces (make-hash-table :test #'eq))   ; reel picture -> its surface
  (free '())                                ; surfaces no picture holds
  (slices '())                              ; the open picture's slices, in order
  (bitstream nil) (bitstream-gpu 0) (bs-fill 0)
  (row nil)                                 ; (intra deblock bsd mpr) scratch GPU addresses
  (mbw 0) (mbh 0) (width 0) (height 0)
  (out '()))                                ; (picture . surface) decoded, in display order

(defparameter +t4v-bitstream-bytes+ (* 512 1024))   ; one picture's slices; 128 of the window's 1024 pages

(defun %t4v-surface-for (d pic)
  "A surface for reel picture PIC: one no live picture holds, else a new one."
  (let ((live (append (reel.h264::h264-refs (t4vd-reel d)) (reel.h264::h264-pending (t4vd-reel d))
                      (mapcar #'car (t4vd-out d)))))
    (maphash (lambda (p s) (unless (or (eq p pic) (member p live)) (push s (t4vd-free d)) (remhash p (t4vd-surfaces d))))
             (t4vd-surfaces d))
    (setf (gethash pic (t4vd-surfaces d))
          (or (pop (t4vd-free d)) (t4v-surface-new (t4vd-mbw d) (t4vd-mbh d))))))

(defvar *t4v-rows* '() "(mbw . row stores), shared by every decoder.")
(defvar *t4v-bs* nil "(gpu . cpu) of the bitstream buffer, shared by every decoder.")

(defun %t4v-ensure (d sps)
  (let ((mbw (reel.h264::sps-mb-width sps)) (mbh (reel.h264::sps-mb-height sps)))
    (unless (and (= mbw (t4vd-mbw d)) (= mbh (t4vd-mbh d)))
      (setf (t4vd-mbw d) mbw (t4vd-mbh d) mbh (t4vd-free d) '())
      (clrhash (t4vd-surfaces d))
      (setf (t4vd-row d)
            (or (cdr (assoc mbw *t4v-rows*))
                (let ((rows (list (t4g-alloc (* mbw 64) :map nil) (t4g-alloc (* mbw 64 4) :map nil)
                                  (t4g-alloc (* mbw 96) :map nil) (t4g-alloc (* mbw 64) :map nil))))
                  (push (cons mbw rows) *t4v-rows*)
                  rows))))
    (setf (t4vd-width d) (reel.h264::sps-width sps) (t4vd-height d) (reel.h264::sps-height sps))
    (unless *t4v-bs*
      (multiple-value-bind (g c) (t4g-alloc +t4v-bitstream-bytes+) (setq *t4v-bs* (cons g c))))
    (setf (t4vd-bitstream-gpu d) (car *t4v-bs*) (t4vd-bitstream d) (cdr *t4v-bs*))))

(defun t4v-release (d)
  "Give D's surfaces back to the pool (its pictures are done with)."
  (maphash (lambda (p s) (declare (ignore p)) (push s *t4v-pool*)) (t4vd-surfaces d))
  (setq *t4v-pool* (append (t4vd-free d) *t4v-pool*))
  (clrhash (t4vd-surfaces d))
  (setf (t4vd-free d) '()))

(defun %t4v-put-bytes (d bytes start end)
  "Copy BYTES[START..END) into the bitstream buffer; its offset there."
  (let ((off (* 64 (ceiling (t4vd-bs-fill d) 64))) (cpu (t4vd-bitstream d)))
    (when (> (+ off (- end start) 64) +t4v-bitstream-bytes+) (error "t420 video: slice data overflow"))
    (loop for i from start below end by 4
          for o from off by 4
          do (t4g-w (+ cpu o) (logior (aref bytes i)
                                      (if (< (+ i 1) end) (ash (aref bytes (+ i 1)) 8) 0)
                                      (if (< (+ i 2) end) (ash (aref bytes (+ i 2)) 16) 0)
                                      (if (< (+ i 3) end) (ash (aref bytes (+ i 3)) 24) 0))))
    (setf (t4vd-bs-fill d) (+ off (- end start)))
    off))

(defun t4v-feed (d bytes start end)
  "Give the decoder the NAL unit BYTES[START..END).  Pictures that became ready to show are pushed
   on (T4VD-OUT D), in display order."
  (let* ((r (t4vd-reel d)) (nal (reel.h264::parse-nal bytes start end)) (type (reel.h264::nal-type nal)))
    (cond
      ((or (= type 7) (= type 8)) (reel.h264::feed-nal r nal))
      ((reel.h264::nal-slice-p nal)
       (let* ((br (reel.h264::make-bitreader (reel.h264::nal-rbsp nal)))
              (sh (reel.h264::parse-slice-header br nal (reel.h264::h264-sps r) (reel.h264::h264-pps r)))
              (sps (reel.h264::sh-sps sh)))
         (when (zerop (reel.h264::sh-first-mb sh))
           (t4v-end-picture d)
           (when (reel.h264::nal-idr-p nal) (reel.h264::%drain r t) (%t4v-collect d))
           (%t4v-ensure d sps)
           (let ((pic (reel.h264::make-picture)))
             (setf (reel.h264::pic-poc pic) (reel.h264::%picture-order-count r sh nal)
                   (reel.h264::pic-frame-num pic) (reel.h264::sh-frame-num sh)
                   (reel.h264::pic-ref-p pic) (plusp (reel.h264::nal-ref-idc nal))
                   (reel.h264::h264-picture r) pic
                   (reel.h264::h264-reorder r) (reel.h264::%reorder-depth sps))
             (%t4v-surface-for d pic)))
         (setf (reel.h264::h264-cur-sh r) sh)
         (multiple-value-bind (l0 l1)
             (cond ((reel.h264::sh-b-slice-p sh) (reel.h264::build-ref-lists-b r sh))
                   ((reel.h264::sh-p-slice-p sh) (values (reel.h264::build-ref-list-0 r sh) nil))
                   (t (values nil nil)))
           (let* ((bit (reel.h264::br-bit br))
                  (bit (if (reel.h264::pps-cabac (reel.h264::sh-pps sh)) (* 8 (ceiling bit 8)) bit))
                  (raw (%t4v-raw-offset bytes start (ash bit -3)))
                  (off (%t4v-put-bytes d bytes start end)))
             (setf (t4vd-slices d)
                   (append (t4vd-slices d)
                           (list (list :sh sh :l0 l0 :l1 l1 :off off :size (- end start)
                                       :mb-byte raw :mb-bit (logand bit 7))))))))))))

(defun %t4v-collect (d)
  "Move reel's ready pictures onto (T4VD-OUT D) with their surfaces."
  (let ((r (t4vd-reel d)))
    (loop for p = (pop (reel.h264::h264-out r)) while p
          do (setf (t4vd-out d) (append (t4vd-out d) (list (cons p (gethash p (t4vd-surfaces d)))))))))

(defun t4v-flush (d)
  "The end of the stream: the last picture decoded, and everything held handed out."
  (t4v-end-picture d)
  (reel.h264::%drain (t4vd-reel d) t)
  (%t4v-collect d))

;;; ---- one picture, on the engine ---------------------------------------------------------------

(defconstant +mfx-pipe-mode+     #x70000000)   ; MFX(2,0,0,0)
(defconstant +mfx-surface+       #x70010000)
(defconstant +mfx-pipe-buf+      #x70020000)
(defconstant +mfx-ind-obj+       #x70030000)
(defconstant +mfx-bsp-buf+       #x70040000)
(defconstant +mfx-avc-img+       #x71000000)   ; MFX(2,1,0,0)
(defconstant +mfx-avc-qm+        #x71010000)
(defconstant +mfx-avc-direct+    #x71020000)
(defconstant +mfx-avc-slice+     #x71030000)
(defconstant +mfx-avc-ref-idx+   #x71040000)
(defconstant +mfx-avc-weight+    #x71050000)
(defconstant +mfd-avc-bsd+       #x71280000)   ; MFX(2,1,1,8)

(defun t4v-end-picture (d)
  "Decode the open picture on the engine, then mark references and reorder as reel does."
  (let* ((r (t4vd-reel d)) (pic (reel.h264::h264-picture r)) (sh (reel.h264::h264-cur-sh r)))
    (when (and pic sh (t4vd-slices d))
      (%t4v-decode d pic)
      (when (reel.h264::nal-idr-p (reel.h264::sh-nal sh)) (setf (reel.h264::h264-refs r) '()))
      (when (plusp (reel.h264::nal-ref-idc (reel.h264::sh-nal sh)))
        (reel.h264::%mark-references r sh (reel.h264::sh-sps sh)))
      (push pic (reel.h264::h264-pending r))
      (reel.h264::%drain r)
      (%t4v-collect d))
    (setf (reel.h264::h264-picture r) nil (reel.h264::h264-cur-sh r) nil
          (t4vd-slices d) '() (t4vd-bs-fill d) 0)))

(defun %t4v-slice (d ring store cur pps mbw mbh poc s next-s)
  "One slice's commands: DIRECTMODE, SLICE_STATE, REF_IDX, WEIGHTOFFSET, BSD_OBJECT."
  (flet ((out (&rest ds) (t4g-emit ring ds))
         (fs (p) (position p store))
         (s2 (x) (logand x #xFFFF)))
    (let* ((sh (getf s :sh)) (next (and next-s (getf next-s :sh)))
          (type (cond ((reel.h264::sh-i-slice-p sh) 2) ((reel.h264::sh-p-slice-p sh) 0) (t 1)))
          (l0 (getf s :l0)) (l1 (getf s :l1))
          (n0 (if (and (/= type 2) (plusp (length store))) (reel.h264::sh-num-ref-idx-l0 sh) 0))
          (n1 (if (and (= type 1) (plusp (length store))) (reel.h264::sh-num-ref-idx-l1 sh) 0))
          (wp (cond ((= type 0) (if (reel.h264::pps-weighted-pred pps) 1 0))
                    ((= type 1) (reel.h264::pps-weighted-bipred pps)) (t 0)))
          (ld (if (= wp 2) 5 (reel.h264::sh-luma-log2-denom sh)))
          (cd (if (= wp 2) 5 (reel.h264::sh-chroma-log2-denom sh)))
          (first-mb (reel.h264::sh-first-mb sh)))
     ;; DIRECTMODE: every reference's and this picture's MV buffer, and their POCs
     (apply #'out (logior +mfx-avc-direct+ 67)
            (append
             (loop for i below 16
                   append (if (< i (length store))
                              (let ((m (t4vs-dmv (gethash (aref store i) (t4vd-surfaces d))))) (list m m))
                              (list 0 0)))
             (list (t4vs-dmv cur) (t4vs-dmv cur))
             (loop for i below 16
                   append (if (< i (length store))
                              (let ((pp (logand (reel.h264::pic-poc (aref store i)) #xFFFFFFFF)))
                                (list pp pp))
                              (list 0 0)))
             (list (logand poc #xFFFFFFFF) (logand poc #xFFFFFFFF))))
     (out (logior +mfx-avc-slice+ 9) type
          (logior (ash n1 24) (ash n0 16) (ash cd 8) ld)
          (logior (ash wp 30)
                  (if (reel.h264::sh-direct-spatial sh) (ash 1 29) 0)
                  (ash (reel.h264::sh-disable-deblocking sh) 27)
                  (ash (reel.h264::sh-cabac-init-idc sh) 24)
                  (ash (reel.h264::sh-qp sh) 16)
                  (ash (logand (floor (reel.h264::sh-beta-offset sh) 2) #xF) 8)
                  (logand (floor (reel.h264::sh-alpha-offset sh) 2) #xF))
          (logior (ash (floor first-mb mbw) 24) (ash (mod first-mb mbw) 16) first-mb)
          (if next
              (let ((nm (reel.h264::sh-first-mb next)))
                (logior (ash (floor nm mbw) 16) (mod nm mbw)))
              (ash mbh 16))
          (if next 0 (ash 1 19)) 0 0 0 0)
     ;; REF_IDX: list entries name frame-store indices (short-term frames: #x20 | i<<1)
     (flet ((ref-idx (list-no refs n)
              (let ((b (make-array 32 :initial-element #xFF)))
                (loop for p in (coerce refs 'list) for i from 0 below (min 32 n)
                      do (let ((k (fs p))) (setf (aref b i) (if k (logior #x20 (ash k 1)) #xFF))))
                (apply #'out (logior +mfx-avc-ref-idx+ 8) list-no
                       (loop for i from 0 below 32 by 4
                             collect (logior (aref b i) (ash (aref b (+ i 1)) 8)
                                             (ash (aref b (+ i 2)) 16) (ash (aref b (+ i 3)) 24)))))))
       (when (plusp n0) (ref-idx 0 l0 n0))
       (when (plusp n1) (ref-idx 1 l1 n1)))
     ;; WEIGHTOFFSET: explicit weights, 32 references x (Y w o, Cb w o, Cr w o) as shorts
     (flet ((weights (table lw cw)
              (let ((shorts (make-array 192 :initial-element 0)))
                (dotimes (j 32)
                  (when (and lw (< j (array-dimension lw 0)))
                    (setf (aref shorts (* 6 j)) (aref lw j 0) (aref shorts (+ (* 6 j) 1)) (aref lw j 1)
                          (aref shorts (+ (* 6 j) 2)) (aref cw j 0 0) (aref shorts (+ (* 6 j) 3)) (aref cw j 0 1)
                          (aref shorts (+ (* 6 j) 4)) (aref cw j 1 0) (aref shorts (+ (* 6 j) 5)) (aref cw j 1 1))))
                (apply #'out (logior +mfx-avc-weight+ 96) table
                       (loop for i from 0 below 192 by 2
                             collect (logior (s2 (aref shorts i)) (ash (s2 (aref shorts (1+ i))) 16)))))))
       (when (and (= type 0) (= wp 1))
         (weights 0 (reel.h264::sh-luma-weights sh) (reel.h264::sh-chroma-weights sh)))
       (when (and (= type 1) (= wp 1))
         (weights 0 (reel.h264::sh-luma-weights sh) (reel.h264::sh-chroma-weights sh))
         (weights 1 (reel.h264::sh-luma-weights-l1 sh) (reel.h264::sh-chroma-weights-l1 sh))))
     ;; BSD_OBJECT: the slice's raw NAL, and where its first macroblock starts
     (out (logior +mfd-avc-bsd+ 4) (getf s :size) (getf s :off) 0
          (logior (ash (getf s :mb-byte) 16) (ash 1 7) (ash 1 6) (- 7 (getf s :mb-bit))) 0))))

(defvar *t4v-batch* nil "A picture's commands: a ring-shaped view of a 64 KB batch buffer.")

(defun %t4v-batch ()
  "The batch, emptied."
  (unless *t4v-batch*
    (multiple-value-bind (g c) (t4g-alloc 65536)
      (setq *t4v-batch* (make-t4g-ring :gpu g :cpu c :size 65536 :name "video batch"))))
  (setf (t4r-tail *t4v-batch*) 0)
  *t4v-batch*)

(defun %t4v-decode (d pic)
  (let* ((r (t4vd-reel d)) (vcs (t4v-start)) (ring (%t4v-batch))
         (cur (gethash pic (t4vd-surfaces d)))
         (sh0 (getf (first (t4vd-slices d)) :sh))
         (sps (reel.h264::sh-sps sh0)) (pps (reel.h264::sh-pps sh0))
         (mbw (t4vd-mbw d)) (mbh (t4vd-mbh d))
         ;; the frame store: every reference picture gets an index for this picture's commands
         ;; (an IDR picture has none: reel forgets the old ones after it is decoded)
         (store (if (reel.h264::nal-idr-p (reel.h264::sh-nal sh0))
                    (vector)
                    (coerce (subseq (reel.h264::h264-refs r) 0 (min 16 (length (reel.h264::h264-refs r)))) 'vector)))
         (deblock (some (lambda (s) (/= 1 (reel.h264::sh-disable-deblocking (getf s :sh)))) (t4vd-slices d))))
    (flet ((out (&rest ds) (t4g-emit ring ds))
           (fs (p) (position p store))
           (s2 (x) (logand x #xFFFF)))
      (out (ash #x26 23) 0 0 0)                                   ; MI_FLUSH_DW
      (out (logior +mfx-pipe-mode+ 2)                             ; VLD decode, AVC
           (logior (if deblock (ash 1 9) (ash 1 8)) 2)
           (logior (ash 1 6) 1) 0)
      (out (logior +mfx-surface+ 4) 0
           (logior (ash (- (t4vs-h cur) 1) 19) (ash (- (t4vs-w cur) 1) 6))
           (logior (ash 4 28) (ash 1 27) (ash (- (t4vs-pitch cur) 1) 3) (ash 1 1) 1)
           (t4vs-h32 cur) 0)
      (apply #'out (logior +mfx-pipe-buf+ 22)
             (if deblock 0 (t4vs-gpu cur)) (if deblock (t4vs-gpu cur) 0) 0 0
             (first (t4vd-row d)) (second (t4vd-row d))
             (append (loop for i below 16
                           collect (if (< i (length store)) (t4vs-gpu (gethash (aref store i) (t4vd-surfaces d))) 0))
                     (list 0)))
      (out (logior +mfx-bsp-buf+ 2) (third (t4vd-row d)) (fourth (t4vd-row d)) 0)
      (out (logior +mfx-avc-img+ 11) (logand (* mbw mbh) #x7FFF) (logior (ash mbh 16) mbw)
           (logior (ash (logand (or (reel.h264::pps-second-chroma-qp-offset pps)
                                    (reel.h264::pps-chroma-qp-offset pps)) #x1F) 24)
                   (ash (logand (reel.h264::pps-chroma-qp-offset pps) #x1F) 16)
                   (ash 1 12) (if (reel.h264::sh-scale-4x4 sh0) (ash 1 10) 0) 16)
           (logior (ash (reel.h264::sps-chroma-format sps) 10)
                   (if (reel.h264::pps-cabac pps) (ash 1 7) 0)
                   (if (reel.h264::pic-ref-p pic) 0 (ash 1 6))
                   (if (reel.h264::pps-constrained-intra pps) (ash 1 5) 0)
                   (if (reel.h264::sps-direct-8x8 sps) (ash 1 4) 0)
                   (if (reel.h264::pps-transform-8x8 pps) (ash 1 3) 0)
                   (ash 1 2))                                     ; frame_mbs_only: frames only
           0 0 0 0 0 0 0 0)
      (when (reel.h264::sh-scale-4x4 sh0)
        (error "t420 video: scaling matrices are not done yet (the engine's built-in flat ones only)"))
      (out (logior +mfx-ind-obj+ 9) (t4vd-bitstream-gpu d) 0 0 0 0 0 0 0 0 0)
      (let ((slices (t4vd-slices d)) (poc (reel.h264::pic-poc pic)))
        (when (plusp (reel.h264::sh-first-mb (getf (first slices) :sh)))
          (error "t420 video: a picture whose first slice does not start at macroblock 0"))
        (loop for (s . rest) on slices
              do (%t4v-slice d ring store cur pps mbw mbh poc s (and rest (first rest)))))
      ;; the phantom slice after the last: starts past the last macroblock
      (out (logior +mfx-avc-slice+ 9) 0 0 0 (logior (ash mbh 24) (* mbw mbh)) 0 0 0 0 0 0)
      (out (logior +mfd-avc-bsd+ 4) 0 0 0 0 0)
      (out (ash #x0A 23) 0)                                       ; MI_BATCH_BUFFER_END
      (when (> (t4r-tail ring) 65000) (error "t420 video: a picture's commands overflowed the batch"))
      (t4g-emit vcs (list (ash #x31 23) (t4r-gpu ring)))          ; MI_BATCH_BUFFER_START: global GTT
      (let ((seq (t4g-breadcrumb vcs)))
        (t4v-submit vcs)
        (unless (t4g-wait vcs seq 500)
          (error "t420 video: the engine did not finish picture ~D (head ~X tail ~X)"
                 (reel.h264::pic-poc pic) (t4-gpu-reg #x12034) (t4-gpu-reg #x12030)))))
    nil))

;;; ---- checking against the host ------------------------------------------------------------------
;;; A test stream sits on the boot disk at some LBA (the rig writes it past the image).  Each
;;; picture out is hashed plane by plane (FNV-1a, Y U V, cropped) for comparing with what the host
;;; computes from ffmpeg's decode of the same stream.

(defun t4v-disk-bytes (lba n)
  "N bytes from the boot disk at LBA."
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (loop for at from 0 below n by (* 64 512)
          for l from lba by 64
          do (unless (%t4-disk-read l 64) (error "t420 video: disk read at ~D failed" l))
             (dotimes (i (min (* 64 512) (- n at)))
               (setf (aref v (+ at i)) (mem-ref (+ #x30000 i) :u8))))
    v))

(defvar *t4v-last* nil "The last stream's decoder, for looking at afterwards.")

(defun t4v-decode-bytes (bytes &optional (fn (lambda (d i pic s) (declare (ignore d i pic s)))))
  "Decode the Annex B stream BYTES on the engine; FN gets (decoder index picture surface) for each
   picture out, in display order, while its surface is still the picture's.  The count."
  (let ((d (make-t4v-dec)) (k 0))
    (flet ((drain ()
             (loop for (p . s) = (pop (t4vd-out d)) while p
                   do (funcall fn d k p s) (incf k))))
      (dolist (n (t4v-split-annex-b bytes))
        (t4v-feed d bytes (car n) (cdr n))
        (drain))
      (t4v-flush d)
      (drain))
    (t4v-release d)
    (setq *t4v-last* d)
    k))

(defun t4v-test (lba n &optional (max 1000))
  "Decode the N-byte stream at LBA; log each picture's plane hashes.  The hashes."
  (let ((hashes '()) (t0 (get-internal-real-time)))
    (t4v-decode-bytes (t4v-disk-bytes lba n)
                      (lambda (d i p s)
                        (declare (ignore p))
                        (when (< i max)
                          (let ((h (mapcar #'t4v-fnv (t4v-read-planes s (t4vd-width d) (t4vd-height d)))))
                            (push h hashes)
                            (t4-log "t4v ~D: ~{~8,'0X~^ ~}" i h)))))
    (t4-log "t4v: ~D pictures, ~D ms" (length hashes)
            (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
    (t4-log-flush)
    (nreverse hashes)))
