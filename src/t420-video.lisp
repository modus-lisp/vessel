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

;;; ---- showing a picture: NV12 -> RGB on the render engine ----------------------------------------
;;; The display planes here scan out RGB only, so a decoded picture is drawn: one RECTLIST through
;;; the 3D pipeline, whose pixel shader samples Y (R8) and interleaved UV (R8G8) from the
;;; Y-tiled surface and converts with BT.601.  The shader is intel-vaapi-driver's gen 6 planar
;;; kernel (src/shaders/render: exa_wm_src_affine + _src_sample_planar + _yuv_color_balance +
;;; _yuv_rgb + _write, MIT), the pipeline state as t4g-render-copy's (igt's).  Its constants
;;; (GRF 6..9): the NV12 flag and skip-colour-balance as shorts, colour balance at 4..7, the
;;; YUV->RGB coefficients at 8..19.  Binding table: 0 the target, 1 and 2 Y, 3 and 4 UV;
;;; samplers 0 (Y) and 2 (UV).

(defparameter *t4v-ps-nv12*
  '(
    #x0060005a #x204077be #x00000140 #x008d0040 #x0060005a #x206077be #x00000140 #x008d0080
    #x0060005a #x208077be #x00000150 #x008d0040 #x0060005a #x20a077be #x00000150 #x008d0080
    #x03000010 #x20002d3c #x000000c0 #x00000000 #x00010020 #x34001c00 #x00001400 #x0000000e
    #x00000201 #x20080061 #x00000000 #x0000e000 #x00600201 #x20200022 #x008d0000 #x00000000
    #x02800031 #x22001cc9 #x00000020 #x0a2a0203 #x00000201 #x20080061 #x00000000 #x0000e000
    #x00600201 #x20200022 #x008d0000 #x00000000 #x02800031 #x22401cc9 #x00000020 #x0a2a0405
    #x00000020 #x34001c00 #x00001400 #x00000006 #x00000201 #x20080061 #x00000000 #x0000c000
    #x00600201 #x20200022 #x008d0000 #x00000000 #x02800031 #x22001cc9 #x00000020 #x0a4a0203
    #x00000201 #x20080061 #x00000000 #x0000e000 #x00600201 #x20200022 #x008d0000 #x00000000
    #x02800031 #x21c01cc9 #x00000020 #x0a2a0001 #x01000010 #x20002d3c #x000000c2 #x00010001
    #x00010020 #x34001c00 #x00001400 #x0000001a #x00800040 #x24007fbd #x008d01c0 #xbd808081
    #x00800041 #x240077bd #x008d0400 #x000000d0 #x00800040 #x240077bd #x008d0400 #x000000d4
    #x00800040 #x21c07fbd #x008d0400 #x3d808081 #x00800040 #x24807fbd #x008d0200 #xbf008084
    #x00800040 #x24407fbd #x008d0240 #xbf008084 #x00800001 #x240003fc #x00000000 #x3f008084
    #x00800048 #x240077bc #x008d0440 #x000000dc #x00800048 #x220077bd #x008d0480 #x000000d8
    #x00000041 #x20dc7fbd #x000000dc #xbf800000 #x00800001 #x240003fc #x00000000 #x3f008084
    #x00800048 #x240077bc #x008d0480 #x000000dc #x00800048 #x224077bd #x008d0440 #x000000d8
    #x00800040 #x22c077bd #x008d01c0 #x000000ec #x00800040 #x230077bd #x008d0200 #x000000fc
    #x00800040 #x234077bd #x008d0240 #x0000010c #x00800041 #x240077bc #x008d02c0 #x000000e0
    #x00800048 #x240077bc #x008d0300 #x000000e4 #x80800048 #x21c077bd #x008d0340 #x000000e8
    #x00800041 #x240077bc #x008d02c0 #x000000f0 #x00800048 #x240077bc #x008d0300 #x000000f4
    #x80800048 #x220077bd #x008d0340 #x000000f8 #x00800041 #x240077bc #x008d02c0 #x00000100
    #x00800048 #x240077bc #x008d0300 #x00000104 #x80800048 #x224077bd #x008d0340 #x00000108
    #x00800001 #x228003fd #x00000000 #x3f800000 #x00600001 #x204003be #x008d01c0 #x00000000
    #x00600001 #x206003be #x008d01e0 #x00000000 #x00600001 #x208003be #x008d0200 #x00000000
    #x00600001 #x20a003be #x008d0220 #x00000000 #x00600001 #x20c003be #x008d0240 #x00000000
    #x00600001 #x20e003be #x008d0260 #x00000000 #x00600001 #x210003be #x008d0280 #x00000000
    #x00600001 #x212003be #x008d02a0 #x00000000 #x05800031 #x24001cc8 #x00000040 #x90019000
    #x0000007e #x00000000 #x00000000 #x00000000 #x0000007e #x00000000 #x00000000 #x00000000
    #x0000007e #x00000000 #x00000000 #x00000000 #x0000007e #x00000000 #x00000000 #x00000000
    #x0000007e #x00000000 #x00000000 #x00000000 #x0000007e #x00000000 #x00000000 #x00000000
    #x0000007e #x00000000 #x00000000 #x00000000 #x0000007e #x00000000 #x00000000 #x00000000))

(defparameter *t4v-bt601*                  ; i965_yuv_coefs.c, yuv_to_rgb_bt601
  '(1.164 0.0 1.596 -0.06275  1.164 -0.392 -0.813 -0.50196  1.164 2.017 0.0 -0.50196))

(defvar *t4v-rbatch* nil "(gpu . cpu): the NV12 draw's 8 KB batch.")

(defun %t4v-rw (off v) (t4g-w (+ (cdr *t4v-rbatch*) off) v))

(defun %t4v-ss (off gpu w h pitch format tiled)
  "A SURFACE_STATE at OFF: 2D, FORMAT, W x H, PITCH bytes, Y-tiled when TILED."
  (%t4v-rw off (logior (ash 1 29) (ash format 18) (ash 1 13)))
  (%t4v-rw (+ off 4) gpu)
  (%t4v-rw (+ off 8) (logior (ash (- w 1) 6) (ash (- h 1) 19)))
  (%t4v-rw (+ off 12) (logior (ash (- pitch 1) 3) (if tiled 3 0)))
  (%t4v-rw (+ off 16) 0) (%t4v-rw (+ off 20) 0))

(defun t4v-draw (s width height dst dx dy dw dh)
  "Draw the WIDTH x HEIGHT picture in surface S onto t4g-surface DST at (DX,DY), scaled to
   DW x DH.  The render engine's breadcrumb."
  (unless *t4v-rbatch*
    (multiple-value-bind (g c) (t4g-alloc 8192) (setq *t4v-rbatch* (cons g c)))
    (dotimes (i 2048) (%t4v-rw (* 4 i) 0))
    (loop for d in *t4v-ps-nv12* for o from 4096 by 4 do (%t4v-rw o d))
    (dotimes (i 4)                                                ; samplers: linear, clamp
      (%t4v-rw (+ 2304 (* 16 i)) (logior (ash 1 14) (ash 1 17)))
      (%t4v-rw (+ 2304 (* 16 i) 4) (logior 2 (ash 2 3) (ash 2 6))))
    (%t4v-rw 2400 #xF99A130C) (%t4v-rw 2404 #x799A130C)           ; CC viewport
    (%t4v-rw 2432 (logior #x11 (ash 1 5) (ash 1 31))) (%t4v-rw 2436 3)   ; blend: off
    (%t4v-rw 3072 #x00010001)                                     ; NV12, skip colour balance
    (loop for f in '(1.0 0.0 1.0 0.0) for o from 3088 by 4 do (%t4v-rw o (t4g-f32 f)))
    (loop for f in *t4v-bt601* for o from 3104 by 4 do (%t4v-rw o (t4g-f32 f))))
  (let* ((b (car *t4v-rbatch*)) (p 0) (uv (+ (t4vs-gpu s) (* (t4vs-pitch s) (t4vs-h32 s)))))
    ;; binding table and surfaces
    (loop for i below 5 do (%t4v-rw (+ 2048 (* 4 i)) (+ 2112 (* 32 i))))
    (%t4v-ss 2112 (t4s-gpu dst) (t4s-w dst) (t4s-h dst) (t4s-pitch dst) #x0C0 nil)
    (%t4v-ss 2144 (t4vs-gpu s) width height (t4vs-pitch s) #x140 t)
    (%t4v-ss 2176 (t4vs-gpu s) width height (t4vs-pitch s) #x140 t)
    (%t4v-ss 2208 uv (ash width -1) (ash height -1) (t4vs-pitch s) #x106 t)
    (%t4v-ss 2240 uv (ash width -1) (ash height -1) (t4vs-pitch s) #x106 t)
    (flet ((out (&rest ds) (dolist (d ds) (%t4v-rw p d) (incf p 4))))
      (out #x69040000)                                            ; PIPELINE_SELECT 3D
      (out (logior #x790D0000 1) 0 0)                             ; MULTISAMPLE
      (out #x78180000 1)                                          ; SAMPLE_MASK
      (out (logior #x61010000 8) 0 (logior b 1) (logior b 1) 0 (logior b 1) 0 1 0 1)   ; STATE_BASE_ADDRESS
      (out #x61020000 0)                                          ; STATE_SIP
      (out (logior #x78050000 1) 24 0)                            ; URB
      (out (logior #x780D0000 (ash 1 12) 2) 0 0 2400)             ; VIEWPORT_STATE_POINTERS (CC)
      (out (logior #x78150000 3) 0 0 0 0)                         ; CONSTANT_VS
      (out (logior #x78100000 4) 0 0 0 0 0)                       ; VS: pass-through
      (out (logior #x78160000 3) 0 0 0 0)                         ; CONSTANT_GS
      (out (logior #x78110000 5) 0 0 0 0 0 0)                     ; GS
      (out (logior #x78120000 2) 0 0 0)                           ; CLIP
      (out (logior #x78170000 (ash 1 12) 3) (logior 3072 3) 0 0 0)   ; CONSTANT_PS: 4 registers
      (out (logior #x79050000 5) (logior (ash 7 29) (ash 1 18)) 0 0 0 0 0)   ; null depth
      (out #x79100000 0)                                          ; CLEAR_PARAMS
      (out (logior #x79000000 2) 0 (logior (ash (- (t4s-h dst) 1) 16) (- (t4s-w dst) 1)) 0)
      (out (logior #x780E0000 2) (logior 2432 1) (logior 2560 1) (logior 2560 1))   ; CC_STATE_POINTERS
      (out (logior #x78020000 (ash 1 12) 2) 0 0 2304)             ; SAMPLER_STATE_POINTERS (PS)
      (out (logior #x78130000 18) (logior (ash 1 22) (ash 1 11) (ash 1 4)) 0 (ash 1 29) (ash 2 25)
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)                           ; SF
      (out (logior #x78140000 7) 4096 (logior (ash 1 27) (ash 5 18)) 0 (ash 6 16)
           (logior (ash 39 25) (ash 1 19) (ash 1 1)) (logior (ash 1 20) (ash 1 10)) 0 0)   ; WM
      (out (logior #x78090000 5)                                  ; VERTEX_ELEMENTS: pad, x y, u v
           (logior (ash 1 25) (ash #x000 16)) (logior (ash 2 28) (ash 2 24) (ash 2 20) (ash 2 16))
           (logior (ash 1 25) (ash #x0F6 16)) (logior (ash 1 28) (ash 1 24) (ash 3 20) (ash 3 16))
           (logior (ash 1 25) (ash #x085 16) 4) (logior (ash 1 28) (ash 1 24) (ash 2 20) (ash 2 16)))
      (out (logior #x78010000 (ash 1 12) 2) 0 0 2048)             ; BINDING_TABLE_POINTERS (PS)
      (out (logior #x78080000 3) 12 b (+ b 2047) 0)               ; VERTEX_BUFFERS: pitch 12
      (let ((vstart (* 12 (ceiling (+ p 32) 12))))
        (out (logior #x7B000000 (ash #x0F 10) 4) 3 (/ vstart 12) 1 0 0)   ; RECTLIST
        (out (ash #x0A 23) 0)                                     ; BATCH_BUFFER_END
        (loop for (x y u v) in (list (list (+ dx dw) (+ dy dh) #x3F800000 #x3F800000)
                                     (list dx (+ dy dh) 0 #x3F800000)
                                     (list dx dy 0 0))
              for o from vstart by 12
              do (%t4v-rw o (logior (ash y 16) x)) (%t4v-rw (+ o 4) u) (%t4v-rw (+ o 8) v)))))
  (let ((r *t4g-rcs*))
    (t4g-render-invalidate r)
    (t4g-emit r (list (ash #x31 23) (car *t4v-rbatch*)))
    (let ((seq (t4g-render-breadcrumb r)))
      (t4g-submit r)
      seq)))

;;; ---- a player: the whole screen, until a key -------------------------------------------------

(defun t4v-mpeg2-p (bytes)
  "Does BYTES open as an MPEG-2 video elementary stream (a sequence header)?"
  (and (> (length bytes) 4) (zerop (aref bytes 0)) (zerop (aref bytes 1)) (= 1 (aref bytes 2))
       (= #xB3 (aref bytes 3))))

(defun t4v-play (bytes &key (fps 25) (repeat 1))
  "Play BYTES -- an H.264 Annex B stream, or MPEG-2 video -- full screen at FPS, REPEAT times or until a key or a click.
   (pictures seconds late-frames)."
  (unless *t4g-rcs* (t4g-render-start))
  (unless *t4d*
    (setq *t4d* (list (t4g-surface-new 1366 768) (t4g-surface-new 1366 768) (t4g-surface-new 1366 768))))
  (let* ((a (first *t4d*)) (b (second *t4d*)) (back a) (shown 0) (late 0)
         (hz internal-time-units-per-second) (t0 (get-internal-real-time)))
    (%t4d-input-p)
    (when *t4g-cursor* (t4-gpu-set #x70080 0) (t4-gpu-set #x70084 *t4g-cursor*))
    (dolist (sf (list a b)) (t4g-wait *t4g-blt* (t4g-blt-clear sf 0) 100))
    (unwind-protect
         (catch 't4v-stop
           (flet ((show (w h s)
                    (let* ((scale (min (/ 1366 w) (/ 768 h)))
                           (dw (floor (* w scale))) (dh (floor (* h scale))))
                      (unless (t4g-wait *t4g-rcs* (t4v-draw s w h back (floor (- 1366 dw) 2) (floor (- 768 dh) 2) dw dh) 200)
                        (error "t420 video: the render engine did not finish a picture"))
                      ;; pace: show picture SHOWN at t0 + SHOWN/FPS
                      (let ((due (+ t0 (floor (* shown hz) fps))))
                        (if (< (get-internal-real-time) due)
                            (loop while (< (get-internal-real-time) due))
                            (incf late)))
                      (%t4d-flip back)
                      (setq back (if (eq back a) b a))
                      (incf shown)
                      (when (%t4d-input-p) (throw 't4v-stop nil)))))
             (dotimes (rep repeat)
               (if (t4v-mpeg2-p bytes)
                   (funcall 't4m-decode-bytes bytes
                            (lambda (d i s) (declare (ignore i))
                              (show (funcall 't4md-width d) (funcall 't4md-height d) s)))
                   (t4v-decode-bytes bytes
                                     (lambda (d i p s) (declare (ignore i p))
                                       (show (t4vd-width d) (t4vd-height d) s)))))))
      (t4-gpu-set #x7019C 0)
      (when *t4g-cursor* (t4g-cursor-on *t4-px* *t4-py*)))
    (list shown (float (/ (- (get-internal-real-time) t0) hz)) late)))

;; the rig's test clip: the gadget disk holds it past the image (a 12 s 640x360 Mandelbrot zoom,
;; main profile, B frames); NIL for none
(defvar *t4v-clip* '(144000 . 693507) "(LBA . BYTES) of the clip the desk's Video action plays.")

(defun %t4v-on-desk (desk dw dh)
  "app.lisp's desk-setup hook: Video on the root menu."
  (declare (ignore dw dh))
  (%kiln-desk-register-action
   desk "Video"
   (lambda ()
     (when *t4v-clip*
       (let ((r (t4v-play (t4v-disk-bytes (car *t4v-clip*) (cdr *t4v-clip*)))))
         (t4-log "video: ~D pictures in ~,1F s, ~D late" (first r) (second r) (third r)))))))

(pushnew '%t4v-on-desk *kiln-desk-setup-hooks*)

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
