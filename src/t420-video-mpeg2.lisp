;;;; t420-video-mpeg2.lisp -- MPEG-2 video on the T420's GPU (Sandy Bridge's MFX engine).
;;;;
;;;; The same engine, ring, batch, surfaces and bitstream buffer as t420-video.lisp's H.264; MPEG-2
;;;; asks much less of the driver.  Per picture: PIPE_MODE_SELECT (MPEG-2, no loop filter, so the
;;;; output is the pre-deblocking one), SURFACE_STATE, PIPE_BUF_ADDR (the output and the two
;;;; references), BSP_BUF (one row store), MPEG2_PIC_STATE (the f_codes and the picture coding
;;;; extension's flags), MPEG2_QM_STATE twice (the intra and non-intra weights, raster order), then
;;;; one MPEG2_BSD_OBJECT per slice: where its first macroblock's bits start, its row and column,
;;;; how many macroblocks until the next slice, and its quantiser_scale_code (the engine starts
;;;; past the slice header, so it is told the one thing the header held).  As
;;;; intel-vaapi-driver's gen6_mfd.c emits them.
;;;;
;;;; reel's MPEG-2 headers do the parsing.  Display order is MPEG-2's own: a B picture is shown as
;;;; soon as it is decoded; an I or P picture is shown when the next I or P arrives (or at the end).
;;;; Field pictures too: a field that opens a frame takes a surface, the opposite field that follows
;;;; decodes into it (as the bottom or top rows the engine is told), and the frame is shown and
;;;; retired when both are done.  MPEG-1 streams are refused.

(defconstant +mfx-mpeg2-pic+ #x73000000)   ; MFX(2,3,0,0)
(defconstant +mfx-mpeg2-qm+  #x73010000)   ; MFX(2,3,0,1)
(defconstant +mfd-mpeg2-bsd+ #x73280000)   ; MFX(2,3,1,8)

(defstruct (t4m-dec (:conc-name t4md-))
  seq ph
  cur                           ; the surface being decoded into
  half                          ; a field picture's frame with only its first field decoded: (type . structure)
  first-p                       ; is the open picture the first field of its frame
  last next                     ; the reference surfaces: the older and the newer
  (slices '())                  ; (offset size mb-bit-offset row column qscale-code), in order
  (fill 0)                      ; bytes used in the bitstream buffer
  (width 0) (height 0) (mbw 0) (mbh 0)
  (shown 0) fn)                 ; pictures handed to FN (decoder index surface)

(defun %t4m-rows (mbw)
  (or (cdr (assoc mbw *t4v-rows*))
      (let ((rows (list (t4g-alloc (* mbw 64) :map nil) (t4g-alloc (* mbw 64 4) :map nil)
                        (t4g-alloc (* mbw 96) :map nil) (t4g-alloc (* mbw 64) :map nil))))
        (push (cons mbw rows) *t4v-rows*)
        rows)))

(defun %t4m-put (d bytes start end)
  "Copy BYTES[START..END) into the bitstream buffer; its offset there."
  (unless *t4v-bs*
    (multiple-value-bind (g c) (t4g-alloc +t4v-bitstream-bytes+) (setq *t4v-bs* (cons g c))))
  (let ((off (* 64 (ceiling (t4md-fill d) 64))) (cpu (cdr *t4v-bs*)))
    (when (> (+ off (- end start) 64) +t4v-bitstream-bytes+) (error "t420 video: slice data overflow"))
    (loop for i from start below end by 4
          for o from off by 4
          do (t4g-w (+ cpu o) (logior (aref bytes i)
                                      (if (< (+ i 1) end) (ash (aref bytes (+ i 1)) 8) 0)
                                      (if (< (+ i 2) end) (ash (aref bytes (+ i 2)) 16) 0)
                                      (if (< (+ i 3) end) (ash (aref bytes (+ i 3)) 24) 0))))
    (setf (t4md-fill d) (+ off (- end start)))
    off))

(defun %t4m-slice-header (bytes start end)
  "The slice at BYTES[START..END) (START at its 00 00 01): (values the bit offset of its first
   macroblock from START, quantiser_scale_code, the first macroblock's column)."
  (let ((br (reel.mpeg2::make-br bytes :start start :end end)))
    (reel.mpeg2::read-bits br 16) (reel.mpeg2::read-bits br 16)   ; the start code
    (let ((q (reel.mpeg2::read-bits br 5)))
      (when (= 1 (reel.mpeg2::read-bit br))               ; intra_slice and slice_picture_id
        (reel.mpeg2::read-bits br 8)
        (loop while (= 1 (reel.mpeg2::read-bit br)) do (reel.mpeg2::read-bits br 8)))
      (let ((mb-bits (- (reel.mpeg2::br-pos br) (* 8 start))) (acc 0))
        ;; the first macroblock_address_increment: its column (the engine reads it again itself)
        (loop (let ((sym (reel.mpeg2::%vlc br reel.mpeg2::+mb-incr-table+ reel.mpeg2::+mb-incr-bits+)))
                (cond ((null sym) (error "t420 video: a bad macroblock_address_increment"))
                      ((= sym 34))
                      ((= sym 33) (incf acc 33))
                      (t (return (values mb-bits q (+ acc sym)))))))))))

(defun %t4m-show (d s)
  (when s (funcall (t4md-fn d) d (t4md-shown d) s) (incf (t4md-shown d))))

(defun %t4m-free (s) (when s (push s *t4v-pool*)))

(defun %t4m-end-picture (d)
  "Decode the open picture (a frame, or one field); then, once its frame is whole, show and retire
   as MPEG-2's display order says -- by the coding type of the frame's first field."
  (when (and (t4md-cur d) (t4md-slices d))
    (%t4m-decode d)
    (let* ((cur (t4md-cur d)) (ph (t4md-ph d)) (st (reel.mpeg2::ph-structure ph)))
      (if (and (/= st 3) (t4md-first-p d))
          (setf (t4md-half d) (cons (reel.mpeg2::ph-coding-type ph) st))   ; wait for the pair
          (let ((type (if (/= st 3) (car (t4md-half d)) (reel.mpeg2::ph-coding-type ph))))
            (setf (t4md-half d) nil)
            (if (= type reel.mpeg2::+pic-b+)
                (progn (%t4m-show d cur) (%t4m-free cur))
                (progn (%t4m-show d (t4md-next d))
                       (%t4m-free (t4md-last d))
                       (setf (t4md-last d) (t4md-next d) (t4md-next d) cur)))
            (setf (t4md-cur d) nil)))))
  (unless (t4md-half d) (setf (t4md-cur d) nil))
  (setf (t4md-slices d) '() (t4md-fill d) 0))

(defun %t4m-decode (d)
  (let* ((vcs (t4v-start)) (ring (%t4v-batch)) (seq (t4md-seq d)) (ph (t4md-ph d))
         (cur (t4md-cur d)) (mbw (t4md-mbw d)) (mbh (t4md-mbh d)) (rows (%t4m-rows mbw))
         (type (reel.mpeg2::ph-coding-type ph))
         (st (reel.mpeg2::ph-structure ph)) (field (/= st 3)) (second (and field (not (t4md-first-p d))))
         (fwd (if (= type reel.mpeg2::+pic-b+) (t4md-last d) (t4md-next d)))
         (bwd (if (= type reel.mpeg2::+pic-b+) (t4md-next d) fwd))
         (f (reel.mpeg2::ph-f-code ph))
         (tff (if field
                  (if (eq (not second) (= st 1)) 1 0)
                  (if (reel.mpeg2::ph-top-field-first ph) 1 0)))
         ;; the four frame-store slots (intel-vaapi-driver mpeg2_set_reference_surfaces): 0-1 the
         ;; references a top field is predicted from, 2-3 a bottom field's; a P picture's second
         ;; field may be predicted from its own frame's first field, the opposite parity
         (slots (flet ((half (own-parity)
                         (let ((l (cond ((= type reel.mpeg2::+pic-b+) (list fwd bwd))
                                        ((= type reel.mpeg2::+pic-p+)
                                         (append (and second (= st own-parity) (list cur)) (list fwd)))
                                        (t nil))))
                           (list (or (first l) (first l)) (or (second l) (first l))))))
                  (append (half 2) (if (and (not field) (reel.mpeg2::ph-frame-pred-frame-dct ph))
                                       (half 2) (half 1))))))
    (when (and (/= type reel.mpeg2::+pic-i+) (null fwd) (not (and second (= type reel.mpeg2::+pic-p+))))
      (error "t420 video: a ~A picture with no reference" (reel.mpeg2::picture-type-name type)))
    (flet ((out (&rest ds) (t4g-emit ring ds))
           (gpu (s) (if s (t4vs-gpu s) 0))
           (b01 (x) (if x 1 0)))
      (out (ash #x26 23) 0 0 0)                                   ; MI_FLUSH_DW
      (out (logior +mfx-pipe-mode+ 2) (ash 1 8) (logior (ash 1 6) 1) 0)   ; MPEG-2, pre-deblock out
      (out (logior +mfx-surface+ 4) 0
           (logior (ash (- (t4vs-h cur) 1) 19) (ash (- (t4vs-w cur) 1) 6))
           (logior (ash 4 28) (ash 1 27) (ash (- (t4vs-pitch cur) 1) 3) (ash 1 1) 1)
           (t4vs-h32 cur) 0)
      ;; references: frame-store 0 forward, 1 backward (P: forward again); 2 and 3 the same, as
      ;; the bottom fields' (a frame picture with field prediction reads them there)
      (apply #'out (logior +mfx-pipe-buf+ 22) (t4vs-gpu cur) 0 0 0 0 0
             (append (mapcar #'gpu slots) (make-list 13 :initial-element 0)))
      (out (logior +mfx-bsp-buf+ 2) (third rows) 0 0)
      (out (logior +mfx-mpeg2-pic+ 2)
           (logior (ash (aref f 1 1) 28) (ash (aref f 1 0) 24) (ash (aref f 0 1) 20) (ash (aref f 0 0) 16)
                   (ash (reel.mpeg2::ph-intra-dc-precision ph) 14)
                   (ash st 12)
                   (ash tff 11)
                   (ash (b01 (reel.mpeg2::ph-frame-pred-frame-dct ph)) 10)
                   (ash (b01 (reel.mpeg2::ph-concealment-motion-vectors ph)) 9)
                   (ash (b01 (reel.mpeg2::ph-q-scale-type ph)) 8)
                   (ash (b01 (reel.mpeg2::ph-intra-vlc-format ph)) 7)
                   (ash (b01 (reel.mpeg2::ph-alternate-scan ph)) 6))
           (ash type 9)
           (logior (ash (ceiling (t4md-height d) 16) 16) mbw))
      (loop for m in (list (reel.mpeg2::seq-intra-matrix seq) (reel.mpeg2::seq-non-intra-matrix seq))
            for i from 0
            do (apply #'out (logior +mfx-mpeg2-qm+ 16) i
                      (loop for k from 0 below 64 by 4
                            collect (logior (aref m k) (ash (aref m (+ k 1)) 8)
                                            (ash (aref m (+ k 2)) 16) (ash (aref m (+ k 3)) 24)))))
      (out (logior +mfx-ind-obj+ 9) (car *t4v-bs*) 0 0 0 0 0 0 0 0 0)
      (loop for (s . rest) on (t4md-slices d)
            do (destructuring-bind (off size mbo row col q) s
                 (let* ((next (first rest))
                        (row1 (if next (fourth next) (if field (floor (ceiling (t4md-height d) 16) 2) mbh)))
                        (col1 (if next (fifth next) 0))
                        (count (- (+ (* row1 mbw) col1) (+ (* row mbw) col)))
                        (last (if next 0 1)))
                   (out (logior +mfd-mpeg2-bsd+ 3)
                        (- size (ash mbo -3)) (+ off (ash mbo -3))
                        (logior (ash col 24) (ash row 16) (ash count 8) (ash last 5) (ash last 3)
                                (logand mbo 7))
                        (ash q 24)))))
      (out (ash #x0A 23) 0)                                       ; MI_BATCH_BUFFER_END
      (t4g-emit vcs (list (ash #x31 23) (t4r-gpu ring)))
      (let ((sq (t4g-breadcrumb vcs)))
        (t4v-submit vcs)
        (unless (t4g-wait vcs sq 500)
          (error "t420 video: the engine did not finish an MPEG-2 picture (head ~X tail ~X)"
                 (t4-gpu-reg #x12034) (t4-gpu-reg #x12030)))))))

(defun t4m-decode-bytes (bytes &optional (fn (lambda (d i s) (declare (ignore d i s)))))
  "Decode the MPEG-2 video elementary stream BYTES on the engine; FN gets (decoder index surface)
   for each picture, in display order, while the surface is still the picture's.  The count."
  (let ((d (make-t4m-dec :fn fn))
        (i (reel.mpeg2::find-start-code bytes 0)) (end (length bytes)))
    (flet ((ext (payload next)
             (let* ((br (reel.mpeg2::make-br bytes :start payload :end next))
                    (id (reel.mpeg2::read-bits br 4)))
               (case id
                 (1 (when (t4md-seq d) (reel.mpeg2::parse-sequence-extension br (t4md-seq d))))
                 (3 (when (t4md-seq d) (reel.mpeg2::parse-quant-matrix-extension br (t4md-seq d))))
                 (8 (when (t4md-ph d) (reel.mpeg2::parse-picture-coding-extension br (t4md-ph d))))))))
      (loop while (and i (< (+ i 4) end))
            do (let* ((code (aref bytes (+ i 3)))
                      (next (or (reel.mpeg2::find-start-code bytes (+ i 4)) end))
                      (payload (+ i 4)))
                 (cond
                   ((= code #xB3)
                    (%t4m-end-picture d)
                    (setf (t4md-seq d) (reel.mpeg2::parse-sequence-header
                                        (reel.mpeg2::make-br bytes :start payload :end next))))
                   ((= code #xB5) (ext payload next))
                   ((= code #x00)
                    (%t4m-end-picture d)
                    (setf (t4md-ph d) (reel.mpeg2::parse-picture-header
                                       (reel.mpeg2::make-br bytes :start payload :end next))))
                   ((= code #xB7) (%t4m-end-picture d))
                   ((<= 1 code #xAF)
                    (when (null (t4md-slices d))                    ; a picture's first slice
                      (let ((seq (t4md-seq d)) (ph (t4md-ph d)))
                        (unless (and seq ph (reel.mpeg2::ph-mpeg2-p ph))
                          (error "t420 video: MPEG-1, or a slice before its headers"))
                        (setf (t4md-width d) (reel.mpeg2::seq-width seq)
                              (t4md-height d) (reel.mpeg2::seq-height seq)
                              (t4md-mbw d) (reel.mpeg2::seq-mb-width seq)
                              (t4md-mbh d) (reel.mpeg2::seq-mb-height seq))
                        (let ((st (reel.mpeg2::ph-structure ph)) (half (t4md-half d)))
                          (cond ((and half (/= st 3) (/= st (cdr half)))      ; the pair's second field
                                 (setf (t4md-first-p d) nil))
                                (t (when half                                  ; an unpaired field: drop it
                                     (%t4m-free (t4md-cur d)) (setf (t4md-half d) nil))
                                   (setf (t4md-first-p d) (/= st 3)
                                         (t4md-cur d) (t4v-surface-new (t4md-mbw d) (t4md-mbh d))))))))
                    (multiple-value-bind (mbo q col) (%t4m-slice-header bytes i next)
                      (setf (t4md-slices d)
                            (append (t4md-slices d)
                                    (list (list (%t4m-put d bytes i next) (- next i) mbo (1- code) col q)))))))
                 (setq i (if (< next end) next nil)))))
    (%t4m-end-picture d)
    (%t4m-show d (t4md-next d))
    (%t4m-free (t4md-last d)) (%t4m-free (t4md-next d))
    (t4md-shown d)))

(defun t4m-test (lba n &optional (max 1000))
  "Decode the N-byte MPEG-2 stream at LBA; log each picture's plane hashes.  The hashes."
  (let ((hashes '()) (t0 (get-internal-real-time)))
    (t4m-decode-bytes (t4v-disk-bytes lba n)
                      (lambda (d i s)
                        (when (< i max)
                          (let ((h (mapcar #'t4v-fnv (t4v-read-planes s (t4md-width d) (t4md-height d)))))
                            (push h hashes)
                            (t4-log "t4v ~D: ~{~8,'0X~^ ~}" i h)))))
    (t4-log "t4v: ~D pictures, ~D ms" (length hashes)
            (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
    (t4-log-flush)
    (nreverse hashes)))

;;; ---- checking where bit-exactness is not defined ------------------------------------------------
;;; MPEG-2 (like MPEG-4 Part 2 and VC-1's predecessors) specifies its inverse DCT only to IEEE 1180
;;; accuracy, so two correct decoders may differ by one here and there, and drift along a GOP.
;;; Hashes cannot judge that; this compares with the host's decode, frame by frame, as the largest
;;; difference and the PSNR of each plane.  The reference frames (yuv420p, WIDTH x HEIGHT, packed)
;;; sit on the disk at REF-LBA.

(defun %t4v-disk-range (lba byte n)
  "N bytes of the disk starting BYTE bytes past LBA."
  (let* ((first (+ lba (floor byte 512))) (skip (mod byte 512))
         (v (t4v-disk-bytes first (+ skip n))))
    (subseq v skip (+ skip n))))

(defun %t4v-psnr (sse n)
  (if (zerop sse) 99.0 (* 10 (log (/ (* 255.0 255.0 n) sse) 10))))

(defun t4v-compare (lba n ref-lba &key (max 1000))
  "Decode the N-byte stream at LBA (H.264, MPEG-2 or VC-1) and compare each picture with the reference
   frames at REF-LBA.  Logs and returns, per picture, (max-diff-Y max-U max-V psnr-Y psnr-U psnr-V)."
  (let* ((bytes (t4v-disk-bytes lba n)) (rows '()))
    (flet ((check (w h i s)
             (when (< i max)
               (let* ((got (t4v-read-planes s w h)) (fs (floor (* 3 w h) 2))
                      (ref (%t4v-disk-range ref-lba (* i fs) fs))
                      (offs (list 0 (* w h) (+ (* w h) (floor (* w h) 4))))
                      (r (let ((maxes '()) (psnrs '()))
                           (loop for plane in got for o in offs
                                 do (let ((mx 0) (sse 0))
                                      (dotimes (k (length plane))
                                        (let ((e (abs (- (aref plane k) (aref ref (+ o k))))))
                                          (when (> e mx) (setq mx e))
                                          (incf sse (* e e))))
                                      (push mx maxes) (push (%t4v-psnr sse (length plane)) psnrs)))
                           (append (nreverse maxes) (nreverse psnrs)))))
                 (push r rows)
                 (t4-log "cmp ~D: max ~D ~D ~D  psnr ~,1F ~,1F ~,1F" i
                         (first r) (second r) (third r) (fourth r) (fifth r) (sixth r))))))
      (cond ((t4v-mpeg2-p bytes)
             (t4m-decode-bytes bytes (lambda (d i s) (check (t4md-width d) (t4md-height d) i s))))
            ((t4v-vc1-p bytes)
             (funcall 't4w-decode-bytes bytes
                      (lambda (d i s) (check (funcall 't4wd-width d) (funcall 't4wd-height d) i s))))
            (t (t4v-decode-bytes bytes (lambda (d i p s) (declare (ignore p)) (check (t4vd-width d) (t4vd-height d) i s))))))
    (t4-log "cmp: ~D pictures" (length rows))
    (t4-log-flush)
    (nreverse rows)))
