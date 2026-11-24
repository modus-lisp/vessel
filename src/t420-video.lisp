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

(defun t4v-reset ()
  "Recover the video engine from a hang: reset the media domain (GEN6_GDRST #x941C, bit 2, as
   i915's gen6_reset_engines does) and restart the same ring, empty.  True if the reset finished."
  (let ((r *t4v-vcs*) (done nil))
    (when r
      (t4g-forcewake)
      (t4-gpu-set #x941C 4)
      (dotimes (i 100000) (unless (logtest 4 (t4-gpu-reg #x941C)) (setq done t) (return)))
      (let ((base (t4r-base r)))
        (t4-gpu-set (+ base #x3C) 0)
        (t4-gpu-set (+ base #x34) 0) (t4-gpu-set (+ base #x30) 0)
        (t4-gpu-set (+ base #x2080) (t4r-hws r))
        (t4-gpu-set (+ base #x38) (t4r-gpu r))
        (t4-gpu-set (+ base #x34) 0) (t4-gpu-set (+ base #x30) 0)
        (setf (t4r-tail r) 0)
        (t4-gpu-set (+ base #x3C) (logior (logand (- (t4r-size r) 4096) #x1FF000) 1))))
    done))

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

(defun t4v-read-planes (s width height &optional (cx 0) (cy 0))
  "The decoded picture in S as three octet vectors (Y U V), WIDTH x HEIGHT from luma (CX,CY), the
   crop origin (even)."
  (let* ((rd (%t4v-row-reader s))
         (cw (ash width -1)) (ch (ash height -1))
         (y (make-array (* width height) :element-type '(unsigned-byte 8)))
         (u (make-array (* cw ch) :element-type '(unsigned-byte 8)))
         (v (make-array (* cw ch) :element-type '(unsigned-byte 8))))
    ;; words are read 4-aligned; X is the output column, (+ CX X) the surface's
    (flet ((byte-at (row col) (ldb (byte 8 (* 8 (logand col 3))) (funcall rd row (logandc2 col 3)))))
      (if (and (zerop (logand cx 3)))
          (progn
            (dotimes (r height)
              (loop for x from 0 below width by 4
                    do (let ((d (funcall rd (+ cy r) (+ cx x))))
                         (loop for k from 0 below (min 4 (- width x))
                               do (setf (aref y (+ (* r width) x k)) (ldb (byte 8 (* 8 k)) d))))))
            (dotimes (r ch)
              (loop for x from 0 below width by 4
                    do (let ((d (funcall rd (+ (t4vs-h32 s) (ash cy -1) r) (+ cx x))) (c (ash x -1)))
                         (when (< c cw) (setf (aref u (+ (* r cw) c)) (ldb (byte 8 0) d)
                                              (aref v (+ (* r cw) c)) (ldb (byte 8 8) d)))
                         (when (< (1+ c) cw) (setf (aref u (+ (* r cw) c 1)) (ldb (byte 8 16) d)
                                                   (aref v (+ (* r cw) c 1)) (ldb (byte 8 24) d)))))))
          (progn
            (dotimes (r height)
              (dotimes (x width) (setf (aref y (+ (* r width) x)) (byte-at (+ cy r) (+ cx x)))))
            (dotimes (r ch)
              (dotimes (c cw)
                (let ((row (+ (t4vs-h32 s) (ash cy -1) r)) (col (+ cx (* 2 c))))
                  (setf (aref u (+ (* r cw) c)) (byte-at row col)
                        (aref v (+ (* r cw) c)) (byte-at row (1+ col)))))))))
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

;;; ---- the H.264 decoded picture buffer ---------------------------------------------------------
;;; reel parses the headers; the picture management is here, because the engine decodes fields and
;;; MBAFF frames and reel's own decoder (frames only, short-term only) does not.  A FRAME STORE holds
;;; one frame or a pair of fields; which fields are references and which are decoded are bit masks
;;; (1 top, 2 bottom).  The rules are H.264's 8.2.1 (order counts), 8.2.4 (reference lists) and
;;; 8.2.5 (marking); where the standard leaves room, FFmpeg's reading (h264_refs.c), since it is
;;; the oracle the conformance pictures are checked against.

(defstruct (t4v-fs (:conc-name fs-))
  surface (frame-num 0)
  (poc-top nil) (poc-bot nil)     ; order counts, NIL until that field is decoded
  (ref 0)                         ; fields that are references
  (long nil) (lt-idx 0)
  (decoded 0)                     ; fields that are decoded
  (fake nil)                      ; a frame_num gap's stand-in: borrows a surface, never shown
  (fsid nil)                      ; the engine's frame-store index, kept for the store's life
  (freed nil))

(defun fs-poc (f)
  (let ((a (fs-poc-top f)) (b (fs-poc-bot f)))
    (cond ((and a b) (min a b)) (a a) (b b) (t 0))))

(defstruct (t4v-dec (:conc-name t4vd-))
  (reel (reel.h264::make-decoder))   ; its parameter-set tables
  (short '())                        ; short-term references, most recently marked first
  (long (make-array 16 :initial-element nil))
  (all '())                          ; every frame store made (for giving surfaces back)
  (pending '())                      ; complete, not yet shown
  (cur nil) (st 3) (second nil)      ; the open picture, its structure (3 frame, 1 top, 2 bottom)
  (first-field nil)                  ; (fs . structure): a first field waiting for its pair
  (cur-fno 0) (cur-msb 0) (cur-lsb 0)
  (prev-msb 0) (prev-lsb 0) (prev-fn 0) (prev-fno 0)
  (slices '())
  (bitstream nil) (bitstream-gpu 0) (bs-fill 0)
  (row nil) (mbw 0) (mbh 0) (width 0) (height 0) (reorder 0)
  (crop-x 0) (crop-y 0)              ; the visible picture's origin in the coded one
  (out '()))                         ; (frame store . surface) to show, in display order

(defparameter +t4v-bitstream-bytes+ (* 1024 1024))   ; one picture's slices (a PCM conformance picture is 891 KB); 256 of the window's 1024 pages

(defvar *t4v-rows* '() "(mbw . row stores), shared by every decoder.")
(defvar *t4v-bs* nil "(gpu . cpu) of the bitstream buffer, shared by every decoder.")

(defun %t4v-ensure (d sps)
  (let ((mbw (reel.h264::sps-mb-width sps)) (mbh (reel.h264::sps-mb-height sps)))
    (unless (and (= mbw (t4vd-mbw d)) (= mbh (t4vd-mbh d)))
      (setf (t4vd-mbw d) mbw (t4vd-mbh d) mbh)
      (setf (t4vd-row d)
            (or (cdr (assoc mbw *t4v-rows*))
                (let ((rows (list (t4g-alloc (* mbw 64) :map nil) (t4g-alloc (* mbw 64 4) :map nil)
                                  (t4g-alloc (* mbw 96) :map nil) (t4g-alloc (* mbw 64) :map nil))))
                  (push (cons mbw rows) *t4v-rows*)
                  rows))))
    (setf (t4vd-width d) (reel.h264::sps-width sps) (t4vd-height d) (reel.h264::sps-height sps)
          (t4vd-crop-x d) (* 2 (reel.h264::sps-crop-left sps))
          (t4vd-crop-y d) (* (if (reel.h264::sps-frame-mbs-only sps) 2 4) (reel.h264::sps-crop-top sps))
          (t4vd-reorder d) (reel.h264::%reorder-depth sps))
    (unless *t4v-bs*
      (multiple-value-bind (g c) (t4g-alloc +t4v-bitstream-bytes+) (setq *t4v-bs* (cons g c))))
    (setf (t4vd-bitstream-gpu d) (car *t4v-bs*) (t4vd-bitstream d) (cdr *t4v-bs*))))

(defun %t4h-new-fs (d fn &optional borrow)
  (let ((f (make-t4v-fs :frame-num fn
                        :surface (or borrow (t4v-surface-new (t4vd-mbw d) (t4vd-mbh d)))
                        :fake (and borrow t))))
    (push f (t4vd-all d))
    f))

(defun %t4h-sweep (d)
  "Give back the surfaces of frame stores nothing needs: not references, not waiting to be shown,
   not open."
  (let ((keep (append (t4vd-pending d) (mapcar #'car (t4vd-out d))
                      (list (t4vd-cur d) (car (t4vd-first-field d))))))
    (setf (t4vd-all d)
          (remove-if (lambda (f)
                       (when (and (zerop (fs-ref f)) (not (fs-long f)) (not (member f keep)))
                         (unless (or (fs-fake f) (fs-freed f))
                           (setf (fs-freed f) t) (push (fs-surface f) *t4v-pool*))
                         t))
                     (t4vd-all d)))))

(defun t4v-release (d)
  "Give D's surfaces back to the pool (its pictures are done with)."
  (dolist (f (t4vd-all d))
    (unless (or (fs-fake f) (fs-freed f)) (setf (fs-freed f) t) (push (fs-surface f) *t4v-pool*)))
  (setf (t4vd-all d) '()))

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

;;; -- order counts (8.2.1): (values top bottom), NIL for the field this picture is not

(defun %t4h-poc (d sh nal st)
  (let* ((sps (reel.h264::sh-sps sh)) (idr (reel.h264::nal-idr-p nal))
         (ref (plusp (reel.h264::nal-ref-idc nal)))
         (max-fn (ash 1 (reel.h264::sps-log2-max-frame-num sps))) (fn (reel.h264::sh-frame-num sh))
         (fno (cond (idr 0)
                    ((> (t4vd-prev-fn d) fn) (+ (t4vd-prev-fno d) max-fn))
                    (t (t4vd-prev-fno d)))))
    (setf (t4vd-cur-fno d) fno)
    (flet ((ret (top bot) (case st (3 (values top bot)) (1 (values top nil)) (t (values nil bot)))))
      (case (reel.h264::sps-poc-type sps)
        (0 (let* ((max-lsb (ash 1 (reel.h264::sps-log2-max-poc-lsb sps)))
                  (lsb (reel.h264::sh-poc-lsb sh))
                  (pmsb (if idr 0 (t4vd-prev-msb d))) (plsb (if idr 0 (t4vd-prev-lsb d)))
                  (msb (cond ((and (< lsb plsb) (>= (- plsb lsb) (ash max-lsb -1))) (+ pmsb max-lsb))
                             ((and (> lsb plsb) (> (- lsb plsb) (ash max-lsb -1))) (- pmsb max-lsb))
                             (t pmsb))))
             (setf (t4vd-cur-msb d) msb (t4vd-cur-lsb d) lsb)
             (ret (+ msb lsb) (if (= st 3) (+ msb lsb (reel.h264::sh-delta-poc-bottom sh)) (+ msb lsb)))))
        (1 (let* ((cycle (reel.h264::sps-poc-cycle sps)) (n (length cycle))
                  (abs (if (plusp n) (+ fno fn) 0)) (expd 0)
                  (t2b (reel.h264::sps-offset-for-top-to-bottom sps)) (d0 (reel.h264::sh-delta-poc-0 sh)))
             (when (and (not ref) (plusp abs)) (decf abs))
             (when (plusp abs)
               (setq expd (* (floor (1- abs) n) (reduce #'+ cycle)))
               (loop for c in cycle repeat (1+ (mod (1- abs) n)) do (incf expd c)))
             (unless ref (incf expd (reel.h264::sps-offset-for-non-ref-pic sps)))
             (ret (+ expd d0) (if (= st 3) (+ expd d0 t2b (reel.h264::sh-delta-poc-1 sh)) (+ expd d0 t2b)))))
        (t (let ((p (cond (idr 0) (ref (* 2 (+ fno fn))) (t (1- (* 2 (+ fno fn)))))))
             (ret p p)))))))

;;; -- reference lists (8.2.4).  An entry: (frame-store structure long-p pic-id).

(defun %t4h-init-lists (d sh)
  "The initial lists for the open picture: (values list0 list1)."
  (let* ((st (t4vd-st d)) (cur (t4vd-cur d))
         (longs (loop for i below 16 for f = (aref (t4vd-long d) i) when f collect (cons i f))))
    (flet ((def (items is-long)
             (flet ((fsof (x) (if is-long (cdr x) x))
                    (id (x) (if is-long (car x) (fs-frame-num x))))
               (if (= st 3)
                   (loop for x in items
                         when (= 3 (logand (fs-ref (fsof x)) 3))
                           collect (list (fsof x) 3 is-long (id x)))
                   ;; a field picture: same parity first, then alternating (8.2.4.2.5)
                   (let ((same (remove-if-not (lambda (x) (logtest (fs-ref (fsof x)) st)) items))
                         (opp (remove-if-not (lambda (x) (logtest (fs-ref (fsof x)) (logxor st 3))) items))
                         (out '()))
                     (loop while (or same opp)
                           do (when same (let ((x (pop same)))
                                           (push (list (fsof x) st is-long (1+ (* 2 (id x)))) out)))
                              (when opp (let ((x (pop opp)))
                                          (push (list (fsof x) (logxor st 3) is-long (* 2 (id x))) out))))
                     (nreverse out))))))
      (if (reel.h264::sh-b-slice-p sh)
          (let* ((cp (if (= st 3) (fs-poc cur) (if (= st 1) (fs-poc-top cur) (fs-poc-bot cur))))
                 (short (t4vd-short d))
                 (before (sort (remove-if-not (lambda (f) (<= (fs-poc f) cp)) (copy-list short)) #'> :key #'fs-poc))
                 (after (sort (remove-if-not (lambda (f) (> (fs-poc f) cp)) (copy-list short)) #'< :key #'fs-poc))
                 (l0 (append (def (append before after) nil) (def longs t)))
                 (l1 (append (def (append after before) nil) (def longs t))))
            (when (and (> (length l1) 1) (= (length l0) (length l1))
                       (every (lambda (a b) (eq (first a) (first b))) l0 l1))
              (setq l1 (list* (second l1) (first l1) (cddr l1))))
            (values l0 l1))
          (values (append (def (t4vd-short d) nil) (def longs t)) nil)))))

(defun %t4h-modify (d sh init ops n)
  "List INIT cut to N and modified by OPS (8.2.4.3), as a vector."
  (let* ((sps (reel.h264::sh-sps sh)) (st (t4vd-st d)) (field (/= st 3))
         (max-fn (ash 1 (reel.h264::sps-log2-max-frame-num sps)))
         (max-pic (if field (* 2 max-fn) max-fn))
         (fn (reel.h264::sh-frame-num sh))
         (pred (if field (1+ (* 2 fn)) fn))
         (w (make-array n :initial-element nil)))
    (loop for e in init for i below n do (setf (aref w i) e))
    (loop for (op . val) in ops for index from 0 below n
          do (let ((e (case op
                        ((0 1)
                         (setq pred (logand (if (zerop op) (- pred (1+ val)) (+ pred (1+ val))) (1- max-pic)))
                         (let* ((par (if field (if (oddp pred) st (logxor st 3)) 3))
                                (fnum (if field (ash pred -1) pred))
                                (f (find-if (lambda (f) (and (= (fs-frame-num f) fnum) (logtest (fs-ref f) par)))
                                            (reverse (t4vd-short d)))))
                           (and f (list f par nil pred))))
                        (2 (let* ((par (if field (if (oddp val) st (logxor st 3)) 3))
                                  (idx (if field (ash val -1) val))
                                  (f (and (< idx 16) (aref (t4vd-long d) idx))))
                             (and f (logtest (fs-ref f) par) (list f par t val)))))))
               (when e
                 (let ((i index))
                   (loop while (and (< (1+ i) n)
                                    (not (and (aref w i) (eq (third (aref w i)) (third e))
                                              (= (fourth (aref w i)) (fourth e)))))
                         do (incf i))
                   (loop while (> i index) do (setf (aref w i) (aref w (1- i))) (decf i))
                   (setf (aref w index) e)))))
    ;; a hole (a missing reference): what the list would have started with
    (dotimes (i n) (unless (aref w i) (setf (aref w i) (first init))))
    w))

;;; -- marking (8.2.5)

(defun %t4h-unref (d f mask)
  "Keep only MASK of F's reference fields; drop it from the lists when none are left."
  (setf (fs-ref f) (logand (fs-ref f) mask))
  (when (zerop (fs-ref f))
    (setf (t4vd-short d) (remove f (t4vd-short d)))
    (dotimes (i 16) (when (eq (aref (t4vd-long d) i) f) (setf (aref (t4vd-long d) i) nil)))
    (setf (fs-long f) nil)))

(defun %t4h-remove-long (d i mask)
  (let ((f (aref (t4vd-long d) i)))
    (when f
      (setf (fs-ref f) (logand (fs-ref f) mask))
      (when (zerop (fs-ref f))
        (setf (aref (t4vd-long d) i) nil (fs-long f) nil)))))

(defun %t4h-nlong (d) (count-if #'identity (t4vd-long d)))

(defun %t4h-mark (d sh sps &key (adaptive (reel.h264::sh-adaptive-ref-marking sh)) (ops (reel.h264::sh-mmco sh)))
  "Mark the open picture, a reference, and retire what the stream says (or the window does).
   True when the picture carried memory_management_control_operation 5."
  (let* ((st (t4vd-st d)) (cur (t4vd-cur d)) (field (/= st 3))
         (max-fn (ash 1 (reel.h264::sps-log2-max-frame-num sps)))
         (max-pic (if field (* 2 max-fn) max-fn))
         (curr (if field (1+ (* 2 (fs-frame-num cur))) (fs-frame-num cur)))
         (limit (max 1 (reel.h264::sps-max-ref-frames sps)))
         (assigned nil) (mmco5 nil))
    (flet ((extract (num) (if field (values (ash num -1) (if (oddp num) st (logxor st 3))) (values num 3)))
           (find-short (fnum) (find fnum (t4vd-short d) :key #'fs-frame-num)))
      (cond
        ((reel.h264::nal-idr-p (reel.h264::sh-nal sh))
         (when (reel.h264::sh-long-term-reference sh)
           (setf (aref (t4vd-long d) 0) cur (fs-long cur) t (fs-lt-idx cur) 0
                 (fs-ref cur) (logior (fs-ref cur) st) assigned t)))
        ((not adaptive)
         (when (and (t4vd-short d)
                    (>= (+ (%t4h-nlong d) (length (t4vd-short d))) limit)
                    (not (and field (t4vd-second d) (plusp (fs-ref cur)))))
           (%t4h-unref d (car (last (t4vd-short d))) 0)))
        (t
         (dolist (op ops)
           (case (car op)
             ((1 3)
              (let ((pn (logand (- curr (1+ (if (= 1 (car op)) (cdr op) (cadr op)))) (1- max-pic))))
                (multiple-value-bind (fnum par) (extract pn)
                  (let ((f (find-short fnum)))
                    (when f
                      (if (= 1 (car op))
                          (%t4h-unref d f (logxor par 3))
                          (let ((idx (cddr op)))
                            (unless (eq (aref (t4vd-long d) idx) f) (%t4h-remove-long d idx 0))
                            (setf (t4vd-short d) (remove f (t4vd-short d))
                                  (aref (t4vd-long d) idx) f (fs-long f) t (fs-lt-idx f) idx))))))))
             (2 (multiple-value-bind (idx par) (extract (cdr op))
                  (when (< idx 16) (%t4h-remove-long d idx (logxor par 3)))))
             (6 (let ((idx (cdr op)))
                  (setf (t4vd-short d) (remove cur (t4vd-short d)))
                  (dotimes (j 16) (when (and (eq (aref (t4vd-long d) j) cur) (/= j idx))
                                    (setf (aref (t4vd-long d) j) nil)))
                  (unless (eq (aref (t4vd-long d) idx) cur)
                    (%t4h-remove-long d idx 0)
                    (setf (aref (t4vd-long d) idx) cur (fs-long cur) t (fs-lt-idx cur) idx))
                  (setf (fs-ref cur) (logior (fs-ref cur) st) assigned t)))
             (4 (loop for j from (cdr op) below 16 do (%t4h-remove-long d j 0)))
             (5 (dolist (f (copy-list (t4vd-short d))) (%t4h-unref d f 0))
                (dotimes (j 16) (%t4h-remove-long d j 0))
                (setq mmco5 t))))))
      (unless assigned
        (cond ((eq (first (t4vd-short d)) cur) (setf (fs-ref cur) (logior (fs-ref cur) st)))
              ((fs-long cur) (setf (fs-ref cur) (logior (fs-ref cur) st)))
              (t (let ((old (find-short (fs-frame-num cur)))) (when old (%t4h-unref d old 0)))
                 (push cur (t4vd-short d))
                 (setf (fs-ref cur) (logior (fs-ref cur) st)))))
      (when (> (+ (%t4h-nlong d) (length (t4vd-short d))) limit)
        (if (t4vd-short d)
            (%t4h-unref d (car (last (t4vd-short d))) 0)
            (let ((j (position-if #'identity (t4vd-long d)))) (when j (%t4h-remove-long d j 0)))))
      mmco5)))

;;; -- output: the smallest order count, once more are held than display can run behind

(defun %t4h-bump (d &optional flush)
  (loop while (or (and flush (t4vd-pending d)) (> (length (t4vd-pending d)) (t4vd-reorder d)))
        do (let ((best (first (t4vd-pending d))))
             (dolist (f (t4vd-pending d)) (when (< (fs-poc f) (fs-poc best)) (setq best f)))
             (setf (t4vd-pending d) (remove best (t4vd-pending d)))
             (setf (t4vd-out d) (append (t4vd-out d) (list (cons best (fs-surface best))))))))

(defun %t4h-complete (d f)
  "Frame store F is whole (or an unpaired field that will not be): it waits to be shown."
  (unless (fs-fake f) (push f (t4vd-pending d)))
  (%t4h-bump d))

;;; -- NAL units in

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
           (%t4h-start-picture d sh nal sps))
         (let ((l0 nil) (l1 nil))
           (unless (reel.h264::sh-i-slice-p sh)
             (multiple-value-bind (i0 i1) (%t4h-init-lists d sh)
               (setq l0 (%t4h-modify d sh i0 (reel.h264::sh-ref-list-reordering sh)
                                     (reel.h264::sh-num-ref-idx-l0 sh)))
               (when (reel.h264::sh-b-slice-p sh)
                 (setq l1 (%t4h-modify d sh i1 (reel.h264::sh-ref-list-reordering-l1 sh)
                                       (reel.h264::sh-num-ref-idx-l1 sh))))))
           (let* ((bit (reel.h264::br-bit br))
                  (bit (if (reel.h264::pps-cabac (reel.h264::sh-pps sh)) (* 8 (ceiling bit 8)) bit))
                  ;; in RBSP bytes from the NAL header: gen 6 skips emulation-prevention bytes
                  ;; itself (intel-vaapi-driver gen6_mfd_avc_bsd_object takes the unescaped offset;
                  ;; measured -- an 03 inside a slice header put the raw offset one byte late)
                  (raw (1+ (ash bit -3)))
                  (off (%t4v-put-bytes d bytes start end)))
             (setf (t4vd-slices d)
                   (append (t4vd-slices d)
                           (list (list :sh sh :l0 l0 :l1 l1 :off off :size (- end start)
                                       :mb-byte raw :mb-bit (logand bit 7))))))))))))

(defun %t4h-start-picture (d sh nal sps)
  "A new picture (or field): pair it, fill frame_num gaps, take a frame store, count its order."
  (let* ((st (if (reel.h264::sh-field-pic sh) (if (reel.h264::sh-bottom-field sh) 2 1) 3))
         (fn (reel.h264::sh-frame-num sh)) (idr (reel.h264::nal-idr-p nal))
         (ff (t4vd-first-field d)))
    (%t4v-ensure d sps)
    (setf (t4vd-st d) st (t4vd-second d) nil (t4vd-first-field d) nil)
    (if (and ff (/= st 3) (/= st (cdr ff)) (= fn (fs-frame-num (car ff))))
        (setf (t4vd-cur d) (car ff) (t4vd-second d) t)          ; the pair's second field
        (progn
          (when ff (%t4h-complete d (car ff)))                  ; an unpaired field
          (if idr
              (progn (setf (t4vd-short d) '()) (fill (t4vd-long d) nil)
                     (dolist (f (t4vd-all d)) (setf (fs-ref f) 0 (fs-long f) nil))
                     (%t4h-bump d t)
                     (setf (t4vd-prev-fn d) 0 (t4vd-prev-fno d) 0 (t4vd-prev-msb d) 0 (t4vd-prev-lsb d) 0))
              ;; frame_num gaps: stand-in frames, marked by the sliding window (8.2.5.2)
              (let* ((max-fn (ash 1 (reel.h264::sps-log2-max-frame-num sps)))
                     (prev (t4vd-prev-fn d)))
                (when (and (/= fn prev) (/= fn (mod (1+ prev) max-fn)) (t4vd-short d))
                  (let ((unwrap (if (> prev fn) (- prev max-fn) prev)))
                    (when (> (- fn unwrap) (reel.h264::sps-max-ref-frames sps))
                      (setq prev (mod (- fn (reel.h264::sps-max-ref-frames sps) 1) max-fn))))
                  (loop for g = (mod (1+ prev) max-fn) then (mod (1+ g) max-fn)
                        until (= g fn)
                        do (let ((f (%t4h-new-fs d g (fs-surface (first (t4vd-short d))))))
                             (setf (t4vd-cur d) f (t4vd-st d) 3
                                   (fs-poc-top f) (+ 2 (fs-poc (first (t4vd-short d))))
                                   (fs-poc-bot f) (fs-poc-top f) (fs-decoded f) 3)
                             (%t4h-mark d sh sps :adaptive nil)
                             (let ((old (t4vd-prev-fn d)))
                               (setf (t4vd-prev-fno d) (if (< g old) (+ (t4vd-prev-fno d) max-fn) (t4vd-prev-fno d))
                                     (t4vd-prev-fn d) g))))
                  (setf (t4vd-st d) st))))
          (%t4h-sweep d)
          (setf (t4vd-cur d) (%t4h-new-fs d fn))))
    (multiple-value-bind (top bot) (%t4h-poc d sh nal st)
      (when top (setf (fs-poc-top (t4vd-cur d)) top))
      (when bot (setf (fs-poc-bot (t4vd-cur d)) bot)))))

(defun t4v-end-picture (d)
  "Decode the open picture (frame or field) on the engine, mark it, and see what can be shown."
  (let* ((cur (t4vd-cur d)) (s0 (first (t4vd-slices d))) (sh (and s0 (getf s0 :sh))))
    (when (and cur sh)
      (let* ((nal (reel.h264::sh-nal sh)) (sps (reel.h264::sh-sps sh)) (st (t4vd-st d))
             (ref (plusp (reel.h264::nal-ref-idc nal))) (mmco5 nil))
        (%t4v-decode d cur)
        (setf (fs-decoded cur) (logior (fs-decoded cur) st))
        (when ref (setq mmco5 (%t4h-mark d sh sps)))
        (when mmco5
          ;; the counts start again: what is held goes out first, and this picture is at zero
          (%t4h-bump d t)
          (case st
            (3 (let ((tmp (min (fs-poc-top cur) (fs-poc-bot cur))))
                 (decf (fs-poc-top cur) tmp) (decf (fs-poc-bot cur) tmp)))
            (1 (setf (fs-poc-top cur) 0))
            (2 (setf (fs-poc-bot cur) 0))))
        (when ref
          (if mmco5
              (setf (t4vd-prev-msb d) 0 (t4vd-prev-lsb d) (if (= st 2) 0 (fs-poc-top cur)))
              (setf (t4vd-prev-msb d) (t4vd-cur-msb d) (t4vd-prev-lsb d) (t4vd-cur-lsb d))))
        (setf (t4vd-prev-fn d) (if mmco5 0 (reel.h264::sh-frame-num sh))
              (t4vd-prev-fno d) (if mmco5 0 (t4vd-cur-fno d)))
        (when mmco5 (setf (fs-frame-num cur) 0))
        (if (and (/= st 3) (not (t4vd-second d)))
            (setf (t4vd-first-field d) (cons cur st))
            (%t4h-complete d cur)))))
  (setf (t4vd-cur d) nil (t4vd-slices d) '() (t4vd-bs-fill d) 0))

(defun t4v-flush (d)
  "The end of the stream: the last picture decoded, and everything held handed out."
  (t4v-end-picture d)
  (let ((ff (t4vd-first-field d)))
    (when ff (setf (t4vd-first-field d) nil) (%t4h-complete d (car ff))))
  (%t4h-bump d t))

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

(defun %t4v-slice (d ring store cur pps mbw mbh s next-s)
  "One slice's commands: DIRECTMODE, SLICE_STATE, REF_IDX, WEIGHTOFFSET, BSD_OBJECT.  STORE is
   the picture's frame stores, in the order the engine numbers them."
  (flet ((out (&rest ds) (t4g-emit ring ds))
         (s2 (x) (logand x #xFFFF))
         (u32 (x) (logand (or x 0) #xFFFFFFFF)))
    (let* ((sh (getf s :sh)) (next (and next-s (getf next-s :sh)))
           (sps (reel.h264::sh-sps sh)) (st (t4vd-st d))
           (mbaff (and (= st 3) (reel.h264::sps-mb-adaptive sps)))
           (type (cond ((reel.h264::sh-i-slice-p sh) 2) ((reel.h264::sh-p-slice-p sh) 0) (t 1)))
           (l0 (getf s :l0)) (l1 (getf s :l1))
           (any (some #'identity store))
           (n0 (if (and (/= type 2) any) (reel.h264::sh-num-ref-idx-l0 sh) 0))
           (n1 (if (and (= type 1) any) (reel.h264::sh-num-ref-idx-l1 sh) 0))
           (wp (cond ((= type 0) (if (reel.h264::pps-weighted-pred pps) 1 0))
                     ((= type 1) (reel.h264::pps-weighted-bipred pps)) (t 0)))
           (ld (if (= wp 2) 5 (reel.h264::sh-luma-log2-denom sh)))
           (cd (if (= wp 2) 5 (reel.h264::sh-chroma-log2-denom sh)))
           (first-mb (reel.h264::sh-first-mb sh)))
      ;; DIRECTMODE: every reference's and this picture's MV buffer, and their field order counts
      (apply #'out (logior +mfx-avc-direct+ 67)
             (append
              (loop for f in store
                    append (if f (let ((m (t4vs-dmv (fs-surface f)))) (list m m)) (list 0 0)))
              (list (t4vs-dmv (fs-surface cur)) (t4vs-dmv (fs-surface cur)))
              (loop for f in store
                    append (if f (list (u32 (fs-poc-top f)) (u32 (fs-poc-bot f))) (list 0 0)))
              (list (u32 (fs-poc-top cur)) (u32 (fs-poc-bot cur)))))
      (flet ((pos (mb) (let ((v (floor mb mbw)) (h (mod mb mbw)))
                         (values (if mbaff (* 2 v) v) h))))
        (multiple-value-bind (v h) (pos first-mb)
          (out (logior +mfx-avc-slice+ 9) type
               (logior (ash n1 24) (ash n0 16) (ash cd 8) ld)
               (logior (ash wp 30)
                       (if (reel.h264::sh-direct-spatial sh) (ash 1 29) 0)
                       (ash (reel.h264::sh-disable-deblocking sh) 27)
                       (ash (reel.h264::sh-cabac-init-idc sh) 24)
                       (ash (reel.h264::sh-qp sh) 16)
                       (ash (logand (floor (reel.h264::sh-beta-offset sh) 2) #xF) 8)
                       (logand (floor (reel.h264::sh-alpha-offset sh) 2) #xF))
               (logior (ash v 24) (ash h 16) (logand first-mb #x7FFF))
               (if next
                   (multiple-value-bind (nv nh) (pos (reel.h264::sh-first-mb next)) (logior (ash nv 16) nh))
                   (ash mbh 16))
               (if next 0 (ash 1 19)) 0 0 0 0)))
      ;; REF_IDX: a byte an entry -- long-term (bit 6), a frame (bit 5), the frame store (4:1), the
      ;; bottom field (bit 0); #xFF unused (intel-vaapi-driver get_ref_idx_state_1)
      (flet ((ref-idx (list-no refs n)
               (let ((b (make-array 32 :initial-element #xFF)))
                 (loop for e across refs for i from 0 below (min 32 n)
                       do (let ((k (and e (member (first e) store) (fs-fsid (first e)))))
                            (setf (aref b i)
                                  (if k
                                      (logior (if (third e) #x40 0) (if (= 3 (second e)) #x20 0)
                                              (ash k 1) (if (= 2 (second e)) 1 0))
                                      #xFF))))
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

(defun %t4h-store (d)
  "The engine's sixteen frame-store slots for the open picture: every reference (a second field's
   own first field among them) at its FSID, NIL for an empty slot.  A reference keeps the index it
   was first given for as long as it stays a reference -- the engine writes the indices a picture
   predicted from into its motion-vector buffer, and a later B picture's temporal direct
   prediction reads them back through ITS slots (intel-vaapi-driver
   intel_update_avc_frame_store_index)."
  (let ((refs (remove-duplicates (append (t4vd-short d) (remove nil (coerce (t4vd-long d) 'list)))
                                 :from-end t))
        (slots (make-list 16 :initial-element nil)))
    (dolist (f refs) (when (and (fs-fsid f) (null (nth (fs-fsid f) slots))) (setf (nth (fs-fsid f) slots) f)))
    (dolist (f refs)
      (unless (eq (and (fs-fsid f) (nth (fs-fsid f) slots)) f)
        (let ((i (position nil slots)))
          (when i (setf (fs-fsid f) i (nth i slots) f)))))
    slots))

(defun %t4v-decode (d cur)
  "The open picture -- a frame, an MBAFF frame, or one field of frame store CUR -- on the engine."
  (let* ((vcs (t4v-start)) (ring (%t4v-batch))
         (surf (fs-surface cur)) (st (t4vd-st d)) (field (/= st 3))
         (sh0 (getf (first (t4vd-slices d)) :sh))
         (sps (reel.h264::sh-sps sh0)) (pps (reel.h264::sh-pps sh0))
         (mbaff (and (not field) (reel.h264::sps-mb-adaptive sps)))
         (mbw (t4vd-mbw d)) (mbh (t4vd-mbh d))
         ;; the frame stores this picture may name: every reference (a second field's own first
         ;; field among them), sixteen at most
         (store (%t4h-store d))
         (deblock (some (lambda (s) (/= 1 (reel.h264::sh-disable-deblocking (getf s :sh)))) (t4vd-slices d))))
    (flet ((out (&rest ds) (t4g-emit ring ds)))
      (out (ash #x26 23) 0 0 0)                                   ; MI_FLUSH_DW
      (out (logior +mfx-pipe-mode+ 2)                             ; VLD decode, AVC
           (logior (if deblock (ash 1 9) (ash 1 8)) 2)
           (logior (ash 1 6) 1) 0)
      (out (logior +mfx-surface+ 4) 0
           (logior (ash (- (t4vs-h surf) 1) 19) (ash (- (t4vs-w surf) 1) 6))
           (logior (ash 4 28) (ash 1 27) (ash (- (t4vs-pitch surf) 1) 3) (ash 1 1) 1)
           (t4vs-h32 surf) 0)
      (apply #'out (logior +mfx-pipe-buf+ 22)
             (if deblock 0 (t4vs-gpu surf)) (if deblock (t4vs-gpu surf) 0) 0 0
             (first (t4vd-row d)) (second (t4vd-row d))
             (append (loop for f in store collect (if f (t4vs-gpu (fs-surface f)) 0))
                     (list 0)))
      (out (logior +mfx-bsp-buf+ 2) (third (t4vd-row d)) (fourth (t4vd-row d)) 0)
      ;; AVC_IMG_STATE: sizes are the FRAME's; the structure says which field, if one
      (out (logior +mfx-avc-img+ 11) (logand (* mbw mbh) #x7FFF) (logior (ash mbh 16) mbw)
           (logior (ash (logand (or (reel.h264::pps-second-chroma-qp-offset pps)
                                    (reel.h264::pps-chroma-qp-offset pps)) #x1F) 24)
                   (ash (logand (reel.h264::pps-chroma-qp-offset pps) #x1F) 16)
                   (ash 1 12) (if (reel.h264::sh-scale-4x4 sh0) (ash 1 10) 0)
                   (ash (case st (3 0) (1 1) (t 3)) 8) 16)
           (logior (ash (reel.h264::sps-chroma-format sps) 10)
                   (if (reel.h264::pps-cabac pps) (ash 1 7) 0)
                   (if (plusp (reel.h264::nal-ref-idc (reel.h264::sh-nal sh0))) 0 (ash 1 6))
                   (if (reel.h264::pps-constrained-intra pps) (ash 1 5) 0)
                   (if (reel.h264::sps-direct-8x8 sps) (ash 1 4) 0)
                   (if (reel.h264::pps-transform-8x8 pps) (ash 1 3) 0)
                   (if (reel.h264::sps-frame-mbs-only sps) (ash 1 2) 0)
                   (if mbaff (ash 1 1) 0)
                   (if field 1 0))
           0 0 0 0 0 0 0 0)
      (when (reel.h264::sh-scale-4x4 sh0)
        (error "t420 video: scaling matrices are not done yet (the engine's built-in flat ones only)"))
      (out (logior +mfx-ind-obj+ 9) (t4vd-bitstream-gpu d) 0 0 0 0 0 0 0 0 0)
      (let ((slices (t4vd-slices d)))
        (when (plusp (reel.h264::sh-first-mb (getf (first slices) :sh)))
          (error "t420 video: a picture whose first slice does not start at macroblock 0"))
        (loop for (s . rest) on slices
              do (%t4v-slice d ring store cur pps mbw mbh s (and rest (first rest)))))
      ;; the phantom slice after the last: starts past the last macroblock (of this field)
      (out (logior +mfx-avc-slice+ 9) 0 0 0 (logior (ash mbh 24) (floor (* mbw mbh) (if field 2 1))) 0 0 0 0 0 0)
      (out (logior +mfd-avc-bsd+ 4) 0 0 0 0 0)
      (out (ash #x0A 23) 0)                                       ; MI_BATCH_BUFFER_END
      (when (> (t4r-tail ring) 65000) (error "t420 video: a picture's commands overflowed the batch"))
      (t4g-emit vcs (list (ash #x31 23) (t4r-gpu ring)))          ; MI_BATCH_BUFFER_START: global GTT
      (let ((seq (t4g-breadcrumb vcs)))
        (t4v-submit vcs)
        (unless (t4g-wait vcs seq 500)
          (let ((head (t4-gpu-reg #x12034)) (tail (t4-gpu-reg #x12030)))
            (t4v-reset)
            (error "t420 video: picture ~D hung the engine (head ~X tail ~X)" (fs-poc cur) head tail)))))
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

(defun t4v-draw (s width height dst dx dy dw dh &optional (cx 0) (cy 0))
  "Draw the WIDTH x HEIGHT picture at (CX,CY) in surface S onto t4g-surface DST at (DX,DY), scaled
   to DW x DH.  The render engine's breadcrumb."
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
  (let* ((b (car *t4v-rbatch*)) (p 0) (uv (+ (t4vs-gpu s) (* (t4vs-pitch s) (t4vs-h32 s))))
         (sw (+ cx width)) (sh (+ cy height))      ; the sampled surface reaches the crop's far edge
         (u0 (t4g-f32 (/ cx sw))) (v0 (t4g-f32 (/ cy sh))))
    ;; binding table and surfaces
    (loop for i below 5 do (%t4v-rw (+ 2048 (* 4 i)) (+ 2112 (* 32 i))))
    (%t4v-ss 2112 (t4s-gpu dst) (t4s-w dst) (t4s-h dst) (t4s-pitch dst) #x0C0 nil)
    (%t4v-ss 2144 (t4vs-gpu s) sw sh (t4vs-pitch s) #x140 t)
    (%t4v-ss 2176 (t4vs-gpu s) sw sh (t4vs-pitch s) #x140 t)
    (%t4v-ss 2208 uv (ash sw -1) (ash sh -1) (t4vs-pitch s) #x106 t)
    (%t4v-ss 2240 uv (ash sw -1) (ash sh -1) (t4vs-pitch s) #x106 t)
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
                                     (list dx (+ dy dh) u0 #x3F800000)
                                     (list dx dy u0 v0))
              for o from vstart by 12
              do (%t4v-rw o (logior (ash y 16) x)) (%t4v-rw (+ o 4) u) (%t4v-rw (+ o 8) v)))))
  (let ((r *t4g-rcs*))
    (t4g-render-invalidate r)
    (t4g-emit r (list (ash #x31 23) (car *t4v-rbatch*)))
    (let ((seq (t4g-render-breadcrumb r)))
      (t4g-submit r)
      seq)))

;;; ---- a player: the whole screen, until a key -------------------------------------------------

(defun %t4v-first-code (bytes)
  "The byte after the stream's first 00 00 01 (streams may open with zero padding), or NIL."
  (loop for i from 0 below (min 64 (- (length bytes) 3))
        do (cond ((and (zerop (aref bytes i)) (zerop (aref bytes (1+ i))) (= 1 (aref bytes (+ i 2))))
                  (return (aref bytes (+ i 3))))
                 ((/= 0 (aref bytes i)) (return nil)))))

(defun t4v-mpeg2-p (bytes)
  "Does BYTES open as an MPEG-2 video elementary stream (a sequence header)?"
  (eql #xB3 (%t4v-first-code bytes)))

(defun t4v-vc1-p (bytes)
  "VC-1: an Advanced elementary stream (00 00 01 0F) or an RCV file (0xC5 in its fourth byte)?"
  (or (eql #x0F (%t4v-first-code bytes))
      (and (> (length bytes) 8) (= #xC5 (aref bytes 3)) (zerop (aref bytes 5)))))

(defun t4v-play (bytes &key (fps 25) (repeat 1))
  "Play BYTES -- an H.264 Annex B stream, MPEG-2 video, or VC-1 -- full screen at FPS, REPEAT times or until a key or a click.
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
           (flet ((show (w h s &optional (cx 0) (cy 0))
                    (let* ((scale (min (/ 1366 w) (/ 768 h)))
                           (dw (floor (* w scale))) (dh (floor (* h scale))))
                      (unless (t4g-wait *t4g-rcs* (t4v-draw s w h back (floor (- 1366 dw) 2) (floor (- 768 dh) 2) dw dh cx cy) 200)
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
               (cond
                 ((t4v-mpeg2-p bytes)
                   (funcall 't4m-decode-bytes bytes
                            (lambda (d i s) (declare (ignore i))
                              (show (funcall 't4md-width d) (funcall 't4md-height d) s))))
                 ((t4v-vc1-p bytes)
                   (funcall 't4w-decode-bytes bytes
                            (lambda (d i s) (declare (ignore i))
                              (show (funcall 't4wd-width d) (funcall 't4wd-height d) s))))
                 (t
                   (t4v-decode-bytes bytes
                                     (lambda (d i p s) (declare (ignore i p))
                                       (show (t4vd-width d) (t4vd-height d) s (t4vd-crop-x d) (t4vd-crop-y d)))))))))
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
                          (let ((h (mapcar #'t4v-fnv (t4v-read-planes s (t4vd-width d) (t4vd-height d)
                                                                      (t4vd-crop-x d) (t4vd-crop-y d)))))
                            (push h hashes)
                            (t4-log "t4v ~D: ~{~8,'0X~^ ~}" i h)))))
    (t4-log "t4v: ~D pictures, ~D ms" (length hashes)
            (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
    (t4-log-flush)
    (nreverse hashes)))

(defun %t4v-fnv-word (h w)
  "FNV-1a of H continued over the four bytes of W, low first."
  (dotimes (k 4 h) (setq h (logand (* (logxor h (ldb (byte 8 (* 8 k)) w)) 16777619) #xFFFFFFFF))))

(defun t4v-suite (streams)
  "Decode each (LBA BYTES) stream of STREAMS (H.264, MPEG-2 or VC-1) and log one line for it:
   the picture count and an FNV-1a over every picture's Y, U and V hashes in display order -- the
   host computes the same from its reference decode."
  (dolist (s streams)
    (destructuring-bind (lba n) s
      (let ((agg 2166136261) (k 0) (t0 (get-internal-real-time)))
        (flet ((pic (w h surf &optional (cx 0) (cy 0))
                 (dolist (pl (t4v-read-planes surf w h cx cy))
                   (setq agg (%t4v-fnv-word agg (t4v-fnv pl))))
                 (incf k)))
          (handler-case
              (let ((bytes (t4v-disk-bytes lba n)))
                (cond ((t4v-mpeg2-p bytes)
                       (funcall 't4m-decode-bytes bytes (lambda (d i surf) (declare (ignore i))
                                                          (pic (funcall 't4md-width d) (funcall 't4md-height d) surf))))
                      ((t4v-vc1-p bytes)
                       (funcall 't4w-decode-bytes bytes (lambda (d i surf) (declare (ignore i))
                                                          (pic (funcall 't4wd-width d) (funcall 't4wd-height d) surf))))
                      (t (t4v-decode-bytes bytes (lambda (d i p surf) (declare (ignore i p))
                                                   (pic (t4vd-width d) (t4vd-height d) surf
                                                        (t4vd-crop-x d) (t4vd-crop-y d))))))
                (t4-log "SUITE ~D ~D ~8,'0X ~D ms" lba k agg
                        (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
            (error (e) (t4-log "SUITE ~D ERR after ~D: ~A" lba k e))))
        (t4-log-flush)))))
