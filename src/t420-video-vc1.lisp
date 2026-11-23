;;;; t420-video-vc1.lisp -- VC-1 (and WMV3, its Simple and Main profiles) on the T420's GPU.
;;;;
;;;; The same MFX engine, ring, batch, surfaces and bitstream buffer as H.264 and MPEG-2.  VC-1
;;;; leaves more to the CPU than either: the picture header is parsed here in full -- the
;;;; quantiser, the motion-vector mode, intensity compensation, VOPDQUANT, the transform and table
;;;; selections -- and so are the BITPLANES, the per-macroblock flags (skipped, direct, 4-MV, AC
;;;; prediction, overlap) that a picture header codes in one of seven ways.  The engine wants them
;;;; decoded, four bits a macroblock, two macroblocks a byte.  It then decodes from the first
;;;; macroblock on.  Per picture: PIPE_MODE_SELECT (VC-1), SURFACE_STATE, PIPE_BUF_ADDR,
;;;; BSP_BUF (with the bitplanes), VC1_PIC_STATE, VC1_PRED_PIPE_STATE, VC1_DIRECTMODE_STATE, and
;;;; one VC1_BSD_OBJECT per slice -- as intel-vaapi-driver's gen6_mfd.c, with the fields
;;;; VA-API's VC-1 picture parameters carry, as FFmpeg's vaapi_vc1.c fills them from its parser
;;;; (libavcodec/vc1.c: the header syntax, SMPTE 421M section 7).
;;;;
;;;; Streams: an Advanced-profile elementary stream (start codes 00 00 01 0F/0E/0D/0B, with
;;;; emulation-prevention bytes), or an RCV file (the Simple/Main conformance container: a
;;;; sequence header in its file header, then length-prefixed frames).  Progressive frames only.
;;;; Display order as MPEG-2's: B and BI shown when decoded, I and P when the next reference comes.

(defconstant +mfx-vc1-pic+    #x72000000)   ; MFX(2,2,0,0)
(defconstant +mfx-vc1-pred+   #x72010000)   ; MFX(2,2,0,1)
(defconstant +mfx-vc1-direct+ #x72020000)   ; MFX(2,2,0,2)
(defconstant +mfd-vc1-bsd+    #x72280000)   ; MFX(2,2,1,8)

;;; ---- bits ---------------------------------------------------------------------------------------

(defun %w-bits (br n)
  "N bits, any N (reel's reader takes at most 24 at a time)."
  (if (<= n 24) (reel.mpeg2::read-bits br n)
      (let ((hi (reel.mpeg2::read-bits br (- n 16)))) (logior (ash hi 16) (reel.mpeg2::read-bits br 16)))))
(defun %w-bit (br) (reel.mpeg2::read-bit br))
(defun %w-unary (br stop n)
  "FFmpeg's get_unary: count bits that are not STOP, up to N."
  (let ((i 0)) (loop while (and (< i n) (/= (%w-bit br) stop)) do (incf i)) i))
(defun %w-012 (br) (if (zerop (%w-bit br)) 0 (+ 1 (%w-bit br))))
(defun %w-pos (br) (reel.mpeg2::br-pos br))

(defun %w-vlc-table (codes lens)
  "A hash (len . code) -> index for a short VLC."
  (let ((h (make-hash-table :test #'equal)))
    (loop for c in codes for l in lens for i from 0 do (setf (gethash (cons l c) h) i))
    (cons h (reduce #'max lens))))

(defun %w-vlc (br table)
  (let ((code 0))
    (loop for len from 1 to (cdr table)
          do (setq code (logior (ash code 1) (%w-bit br)))
             (let ((v (gethash (cons len code) (car table)))) (when v (return-from %w-vlc v))))
    (error "t420 video: a bad VC-1 VLC")))

(defparameter *w-imode* (%w-vlc-table '(0 2 1 3 1 2 3) '(4 2 3 2 4 3 3)))
(defparameter *w-norm2* (%w-vlc-table '(0 4 5 3) '(1 3 3 2)))
(defparameter *w-norm6*
  (%w-vlc-table
   '(#x001 #x002 #x003 #x000 #x004 #x001 #x002 #x047 #x005 #x003 #x004 #x04B #x005 #x04D #x04E #x30E
     #x006 #x006 #x007 #x053 #x008 #x055 #x056 #x30D #x009 #x059 #x05A #x30C #x05C #x30B #x30A #x037
     #x007 #x00A #x00B #x043 #x00C #x045 #x046 #x309 #x00D #x049 #x04A #x308 #x04C #x307 #x306 #x036
     #x00E #x051 #x052 #x305 #x054 #x304 #x303 #x035 #x058 #x302 #x301 #x034 #x300 #x033 #x032 #x007)
   '(1 4 4 8 4 8 8 10 4 8 8 10 8 10 10 13  4 8 8 10 8 10 10 13 8 10 10 13 10 13 13 9
     4 8 8 10 8 10 10 13 8 10 10 13 10 13 13 9  8 10 10 13 10 13 13 9 10 13 13 9 13 9 9 6)))

;;; ---- bitplanes (8.7) ------------------------------------------------------------------------------
;;; A plane is an octet vector of W x H (stride W).  Returns (values plane raw-p): RAW-P when the
;;; flags are coded per macroblock instead (the engine reads them there itself).

(defun %w-rowskip (br p w h off stride)
  (dotimes (y h) (if (zerop (%w-bit br))
                     (dotimes (x w) (setf (aref p (+ off (* y stride) x)) 0))
                     (dotimes (x w) (setf (aref p (+ off (* y stride) x)) (%w-bit br))))))

(defun %w-colskip (br p w h off stride)
  (dotimes (x w) (if (zerop (%w-bit br))
                     (dotimes (y h) (setf (aref p (+ off (* y stride) x)) 0))
                     (dotimes (y h) (setf (aref p (+ off (* y stride) x)) (%w-bit br))))))

(defun %w-bitplane (br w h)
  (let* ((p (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 0))
         (invert (%w-bit br)) (imode (%w-vlc br *w-imode*)))
    (case imode
      (0 (return-from %w-bitplane (values p t)))            ; raw: in the macroblock layer
      ((1 2)                                                ; norm-2 / diff-2: pairs, in raster order
       (let ((i 0))
         (when (oddp (* w h)) (setf (aref p 0) (%w-bit br)) (setq i 1))
         (loop while (< i (* w h))
               do (let ((c (%w-vlc br *w-norm2*)))
                    (setf (aref p i) (logand c 1) (aref p (1+ i)) (ash c -1))
                    (incf i 2)))))
      ((3 4)                                                ; norm-6 / diff-6: 2x3 or 3x2 tiles
       (if (and (zerop (mod h 3)) (plusp (mod w 3)))
           (progn
             (loop for y from 0 below h by 3
                   do (loop for x from (logand w 1) below w by 2
                            do (let ((c (%w-vlc br *w-norm6*)))
                                 (dotimes (k 6)
                                   (setf (aref p (+ (* (+ y (floor k 2)) w) x (mod k 2))) (ldb (byte 1 k) c))))))
             (when (oddp w) (%w-colskip br p 1 h 0 w)))
           (progn
             (loop for y from (logand h 1) below h by 2
                   do (loop for x from (mod w 3) below w by 3
                            do (let ((c (%w-vlc br *w-norm6*)))
                                 (dotimes (k 6)
                                   (setf (aref p (+ (* (+ y (floor k 3)) w) x (mod k 3))) (ldb (byte 1 k) c))))))
             (let ((x (mod w 3)))
               (when (plusp x) (%w-colskip br p x h 0 w))
               (when (oddp h) (%w-rowskip br p (- w x) 1 x w))))))
      (5 (%w-rowskip br p w h 0 w))
      (6 (%w-colskip br p w h 0 w)))
    (cond ((member imode '(2 4))                           ; the differential operator
           (setf (aref p 0) (logxor (aref p 0) invert))
           (loop for x from 1 below w do (setf (aref p x) (logxor (aref p x) (aref p (1- x)))))
           (loop for y from 1 below h
                 do (let ((r (* y w)))
                      (setf (aref p r) (logxor (aref p r) (aref p (- r w))))
                      (loop for x from 1 below w
                            do (let ((i (+ r x)))
                                 (setf (aref p i)
                                       (if (/= (aref p (1- i)) (aref p (- i w)))
                                           (logxor (aref p i) invert)
                                           (logxor (aref p i) (aref p (1- i))))))))))
          ((= invert 1) (dotimes (i (* w h)) (setf (aref p i) (- 1 (aref p i))))))
    (values p nil)))

;;; ---- the sequence ---------------------------------------------------------------------------------

(defstruct (w-seq (:conc-name ws-))
  (profile 0) (width 0) (height 0) (loopfilter 0) (multires 0) (res-x8 0) (fastuvmc 0)
  (extended-mv 0) (extended-dmv 0) (dquant 0) (vstransform 0) (overlap 0) (syncmarker 0)
  (rangered 0) (maxbframes 0) (quantizer 0) (finterp 0) (interlace 0) (tfcntr 0) (pulldown 0)
  (postproc 0) (panscan 0) (refdist-flag 0) (hrd 0))

(defun %w-seq-simple-main (br s)
  "Struct C of an RCV file: the Simple/Main sequence header (FFmpeg's ff_vc1_decode_sequence_header)."
  (setf (ws-profile s) (%w-bits br 2))
  (when (= 3 (ws-profile s)) (error "t420 video: an Advanced sequence header in an RCV file"))
  (%w-bit br) (%w-bit br)                                   ; res_y411, res_sprite
  (%w-bits br 3) (%w-bits br 5)                             ; frmrtq, bitrtq
  (setf (ws-loopfilter s) (%w-bit br) (ws-res-x8 s) (%w-bit br) (ws-multires s) (%w-bit br))
  (%w-bit br)                                               ; res_fasttx
  (setf (ws-fastuvmc s) (%w-bit br) (ws-extended-mv s) (%w-bit br) (ws-dquant s) (%w-bits br 2)
        (ws-vstransform s) (%w-bit br))
  (%w-bit br)                                               ; res_transtab
  (setf (ws-overlap s) (%w-bit br) (ws-syncmarker s) (%w-bit br) (ws-rangered s) (%w-bit br)
        (ws-maxbframes s) (%w-bits br 3) (ws-quantizer s) (%w-bits br 2) (ws-finterp s) (%w-bit br))
  s)

(defun %w-seq-advanced (br s)
  "The Advanced sequence header (start code 0F)."
  (setf (ws-profile s) (%w-bits br 2))
  (%w-bits br 3)                                            ; level
  (unless (= 1 (%w-bits br 2)) (error "t420 video: VC-1 chroma other than 4:2:0"))
  (%w-bits br 3) (%w-bits br 5)                             ; frmrtq, bitrtq
  (setf (ws-postproc s) (%w-bit br)
        (ws-width s) (ash (1+ (%w-bits br 12)) 1) (ws-height s) (ash (1+ (%w-bits br 12)) 1)
        (ws-pulldown s) (%w-bit br) (ws-interlace s) (%w-bit br) (ws-tfcntr s) (%w-bit br)
        (ws-finterp s) (%w-bit br))
  (%w-bit br)                                               ; reserved
  (when (= 1 (%w-bit br)) (error "t420 video: VC-1 progressive segmented frames"))
  (when (= 1 (%w-bit br))                                   ; display extension
    (%w-bits br 14) (%w-bits br 14)
    (when (= 1 (%w-bit br)) (when (= 15 (%w-bits br 4)) (%w-bits br 16)))
    (when (= 1 (%w-bit br)) (if (= 1 (%w-bit br)) (%w-bits br 16) (%w-bits br 12)))
    (when (= 1 (%w-bit br)) (%w-bits br 24)))
  (setf (ws-hrd s) 0)
  (when (= 1 (%w-bit br))
    (setf (ws-hrd s) (%w-bits br 5)) (%w-bits br 8)
    (dotimes (i (ws-hrd s)) (%w-bits br 16) (%w-bits br 16)))
  (setf (ws-maxbframes s) 7)
  s)

(defun %w-entry-point (br s)
  "The Advanced entry-point header (start code 0E)."
  (%w-bit br) (%w-bit br)                                   ; broken_link, closed_entry
  (setf (ws-panscan s) (%w-bit br) (ws-refdist-flag s) (%w-bit br) (ws-loopfilter s) (%w-bit br)
        (ws-fastuvmc s) (%w-bit br) (ws-extended-mv s) (%w-bit br) (ws-dquant s) (%w-bits br 2)
        (ws-vstransform s) (%w-bit br) (ws-overlap s) (%w-bit br) (ws-quantizer s) (%w-bits br 2))
  (dotimes (i (ws-hrd s)) (%w-bits br 8))
  (when (= 1 (%w-bit br))
    (setf (ws-width s) (ash (1+ (%w-bits br 12)) 1) (ws-height s) (ash (1+ (%w-bits br 12)) 1)))
  (when (= 1 (ws-extended-mv s)) (setf (ws-extended-dmv s) (%w-bit br)))
  (when (= 1 (%w-bit br)) (%w-bits br 3) (error "t420 video: VC-1 luma range mapping"))
  (when (= 1 (%w-bit br)) (%w-bits br 3) (error "t420 video: VC-1 chroma range mapping"))
  s)

;;; ---- the picture header ---------------------------------------------------------------------------
;;; Picture types as the engine numbers them: 0 I, 1 P, 2 B, 3 BI, 4 skipped P.
;;; MV modes as FFmpeg's: 0 1-MV half-pel bilinear, 1 1-MV, 2 1-MV half-pel, 3 mixed, 4 intensity comp.

(defparameter +w-pquant-implicit+
  #(0 1 2 3 4 5 6 7 8 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 27 29 31))
(defparameter +w-pmode+  #(#(0 1 2 4 3) #(1 3 2 4 0)))
(defparameter +w-pmode2+ #(#(0 1 2 3) #(1 3 2 0)))

(defstruct (w-pic (:conc-name wp-))
  (type 0) (pq 0) (halfqp 0) (pquantizer 0) (mvrange 0) (dmvrange 0) (rnd 0) (rangeredfrm 0)
  (mv-mode 0) (mv-mode2 0) (lumscale 0) (lumshift 0) (bfrac 0) (mvtab 0) (cbptab 0)
  (dquantfrm 0) (dqprofile 0) (dqsbedge 0) (dqbilevel 0) (altpq 0)
  (ttmbf 0) (ttfrm 0) (ac-c 0) (ac-y 0) (dctab 0) (condover 0)
  (planes nil)                  ; (bit . plane) of the nibble's three bits: 0, 1, 2
  (raw '())                     ; which planes are raw: :mvtype :skip :direct :acpred :overflags
                                ; :fieldtx :forward
  ;; interlace (Advanced): frame coding mode 0 progressive, 1 frame-interlaced, 2 field pair
  (fcm 0) (tff 1) (second nil) (fptype 0)
  (numref 0) (reffield 0) (refdist 0) (frfd 0) (brfd 0)
  (fourmvswitch 0) (intcomp 0) (intcompfield 0) (lumscale2 0) (lumshift2 0)
  (mbmodetab 0) (imvtab 0) (icbptab 0) (twomvbptab 0) (fourmvbptab 0)
  (bits 0))                     ; header length in bits

(defun %w-quant (br s p)
  (let ((idx (%w-bits br 5)))
    (when (zerop idx) (error "t420 video: PQINDEX 0"))
    (setf (wp-pq p) (if (zerop (ws-quantizer s)) (aref +w-pquant-implicit+ idx) idx)
          (wp-halfqp p) (if (< idx 9) (%w-bit br) 0)
          (wp-pquantizer p) (case (ws-quantizer s) (0 (if (< idx 9) 1 0)) (1 (%w-bit br)) (2 0) (t 1)))))

(defun %w-vopdquant (br s p)
  (when (/= 2 (ws-dquant s))
    (setf (wp-dquantfrm p) (%w-bit br))
    (when (zerop (wp-dquantfrm p)) (return-from %w-vopdquant))
    (setf (wp-dqprofile p) (%w-bits br 2))
    (case (wp-dqprofile p)
      ((1 2) (setf (wp-dqsbedge p) (%w-bits br 2)))
      (3 (setf (wp-dqbilevel p) (%w-bit br))
         (when (zerop (wp-dqbilevel p)) (setf (wp-halfqp p) 0) (return-from %w-vopdquant)))))
  (let ((d (%w-bits br 3)))
    (setf (wp-altpq p) (if (= d 7) (%w-bits br 5) (+ (wp-pq p) d 1)))))

(defun %w-transform (br s p)
  (if (= 1 (ws-vstransform s))
      (progn (setf (wp-ttmbf p) (%w-bit br))
             (when (= 1 (wp-ttmbf p)) (setf (wp-ttfrm p) (%w-bits br 2))))
      (setf (wp-ttmbf p) 1 (wp-ttfrm p) 0)))

(defun %w-plane (br p mbw mbh bit key)
  (multiple-value-bind (plane raw) (%w-bitplane br mbw mbh)
    (if raw (push key (wp-raw p)) (push (cons bit plane) (wp-planes p)))))

(defun %w-p-or-b-body (br s p mbw mbh)
  "The P or B picture's motion and table fields, from the MV range on."
  (let ((type (wp-type p)))
    (when (= 1 (ws-extended-mv s)) (setf (wp-mvrange p) (%w-unary br 0 3)))
    (when (and (= type 1) (= 3 (ws-profile s)) (= 1 (ws-interlace s)) (= 1 (ws-extended-dmv s)))
      (setf (wp-dmvrange p) (%w-unary br 0 3)))
    (if (= type 1)
        (let ((low (if (> (wp-pq p) 12) 0 1)))
          (setf (wp-mv-mode p) (aref (aref +w-pmode+ low) (%w-unary br 1 4)))
          (when (= 4 (wp-mv-mode p))
            (setf (wp-mv-mode2 p) (aref (aref +w-pmode2+ low) (%w-unary br 1 3))
                  (wp-lumscale p) (%w-bits br 6) (wp-lumshift p) (%w-bits br 6)))
          (when (or (= 3 (wp-mv-mode p)) (and (= 4 (wp-mv-mode p)) (= 3 (wp-mv-mode2 p))))
            (%w-plane br p mbw mbh 2 :mvtype))
          (%w-plane br p mbw mbh 1 :skip))
        (progn
          (setf (wp-mv-mode p) (if (= 1 (%w-bit br)) 1 0))
          (%w-plane br p mbw mbh 0 :direct)
          (%w-plane br p mbw mbh 1 :skip)))
    (setf (wp-mvtab p) (%w-bits br 2) (wp-cbptab p) (%w-bits br 2))
    (when (plusp (ws-dquant s)) (%w-vopdquant br s p))
    (%w-transform br s p)))

(defun %w-tables (br s p)
  (setf (wp-ac-c p) (%w-012 br))
  (when (member (wp-type p) '(0 3)) (setf (wp-ac-y p) (%w-012 br)))
  (setf (wp-dctab p) (%w-bit br))
  (when (and (member (wp-type p) '(0 3)) (plusp (ws-dquant s)) (= 3 (ws-profile s)))
    (%w-vopdquant br s p)))

(defun %w-picture-simple-main (br s mbw mbh last-rnd)
  "A Simple/Main picture header (FFmpeg's ff_vc1_parse_frame_header)."
  (let ((p (make-w-pic)) (start (%w-pos br)))
    (when (= 1 (ws-finterp s)) (%w-bit br))
    (%w-bits br 2)                                          ; FRMCNT
    (when (= 1 (ws-rangered s)) (setf (wp-rangeredfrm p) (%w-bit br)))
    (setf (wp-type p) (cond ((= 1 (%w-bit br)) 1)
                            ((and (plusp (ws-maxbframes s)) (zerop (%w-bit br))) 2)
                            (t 0)))
    (when (= 2 (wp-type p))
      (let ((i (%w-bits br 3))) (when (= i 7) (setq i (+ 7 (%w-bits br 4))))
        (setf (wp-bfrac p) i)
        (when (= i 22) (setf (wp-type p) 3))))
    (when (member (wp-type p) '(0 3)) (%w-bits br 7))       ; buffer fullness
    (setf (wp-rnd p) (case (wp-type p) ((0 3) 1) (1 (logxor last-rnd 1)) (t last-rnd)))
    (%w-quant br s p)
    (when (= 1 (ws-extended-mv s)) (setf (wp-mvrange p) (%w-unary br 0 3)))
    (when (and (= 1 (ws-multires s)) (/= 2 (wp-type p)))
      (unless (zerop (%w-bits br 2)) (error "t420 video: VC-1 multiresolution")))
    (when (and (= 1 (ws-res-x8 s)) (member (wp-type p) '(0 3)) (= 1 (%w-bit br)))
      (error "t420 video: WMV3 X8 intra pictures"))
    (when (member (wp-type p) '(1 2))
      ;; the MV range came above for Simple/Main; the body must not read it again
      (let ((ext (ws-extended-mv s)))
        (setf (ws-extended-mv s) 0)
        (unwind-protect (%w-p-or-b-body br s p mbw mbh) (setf (ws-extended-mv s) ext))))
    (%w-tables br s p)
    (setf (wp-bits p) (- (%w-pos br) start))
    p))

(defparameter +w-bfraction+                ; FFmpeg's ff_vc1_bfraction_lut, BFRACTION as /256
  #(128 85 170 64 192 51 102 153 204 43 215 37 74 111 148 185 222 32 96 160 224 -1 0))

(defun %w-210 (br) (if (= 1 (%w-bit br)) 0 (- 2 (%w-bit br))))

(defun %w-picture-advanced (br s mbw mbh &optional first (anchor-refdist 0))
  "An Advanced picture header (FFmpeg's ff_vc1_parse_frame_header_adv): a progressive picture,
   a frame-interlaced one, or a field.  FIRST, for the header after a field start code (00 00 01
   0C): the first field's, whose frame-level fields the second inherits; its own header begins at
   the quantiser.  Bitplanes of a field are half the frame's rows."
  (let ((p (make-w-pic)) (start (%w-pos br)) (low 0))
    (flet ((plane (bit key) (%w-plane br p mbw (if (= 2 (wp-fcm p)) (floor mbh 2) mbh) bit key))
           (bfraction ()
             (let ((i (%w-bits br 3))) (when (= i 7) (setq i (+ 7 (%w-bits br 4))))
               (setf (wp-bfrac p) i)
               i)))
      (if first
          (let ((fp (wp-fptype first)))
            (setf (wp-fcm p) 2 (wp-second p) t (wp-fptype p) fp (wp-tff p) (wp-tff first)
                  (wp-rnd p) (wp-rnd first) (wp-refdist p) (wp-refdist first)
                  (wp-bfrac p) (wp-bfrac first) (wp-frfd p) (wp-frfd first) (wp-brfd p) (wp-brfd first)
                  (wp-type p) (if (logbitp 2 fp) (if (logbitp 0 fp) 3 2) (if (logbitp 0 fp) 1 0))))
          (progn
            (setf (wp-fcm p) (if (= 1 (ws-interlace s)) (%w-012 br) 0))
            (if (= 2 (wp-fcm p))
                (let ((fp (%w-bits br 3)))
                  (setf (wp-fptype p) fp
                        (wp-type p) (if (logbitp 2 fp) (if (logbitp 1 fp) 3 2) (if (logbitp 1 fp) 1 0))))
                (setf (wp-type p) (aref #(1 2 0 3 4) (%w-unary br 0 4))))
            (when (= 1 (ws-tfcntr s)) (%w-bits br 8))
            (if (= 1 (ws-pulldown s))
                (if (zerop (ws-interlace s)) (%w-bits br 2)
                    (progn (setf (wp-tff p) (%w-bit br)) (%w-bit br)))
                (setf (wp-tff p) 1))
            (when (= 1 (ws-panscan s)) (error "t420 video: VC-1 pan-scan"))
            (when (= 4 (wp-type p)) (setf (wp-bits p) (- (%w-pos br) start)) (return-from %w-picture-advanced p))
            (setf (wp-rnd p) (%w-bit br))
            (when (= 1 (ws-interlace s)) (%w-bit br))       ; UVSAMP
            (case (wp-fcm p)
              (2 (if (member (wp-type p) '(2 3))
                     (let ((i (bfraction)))
                       (setf (wp-refdist p) anchor-refdist)
                       ;; a B field's distances, from the anchor's REFDIST (the decoder's)
                       (setf (wp-frfd p) (ash (* (max 0 (aref +w-bfraction+ i)) (wp-refdist p)) -8)
                             (wp-brfd p) (max 0 (- (wp-refdist p) (wp-frfd p) 1))))
                     (when (= 1 (ws-refdist-flag s))
                       (let ((r (%w-bits br 2)))
                         (setf (wp-refdist p) (if (= r 3) (+ 3 (%w-unary br 0 14)) r))))))
              (0 (when (= 1 (ws-finterp s)) (%w-bit br))
                 (when (= 2 (wp-type p))
                   (when (= 22 (bfraction)) (setf (wp-type p) 3)))))))
      ;; the common part: what every picture and every field has
      (%w-quant br s p)
      (when (= 1 (ws-postproc s)) (%w-bits br 2))
      (setq low (if (> (wp-pq p) 12) 0 1))
      (let ((fcm (wp-fcm p)) (field (= 2 (wp-fcm p))))
        (case (wp-type p)
          ((0 3)
           (when (= 1 fcm) (plane 0 :fieldtx))
           (plane 1 :acpred)
           (when (and (= 1 (ws-overlap s)) (<= (wp-pq p) 8))
             (setf (wp-condover p) (%w-012 br))
             (when (= 2 (wp-condover p)) (plane 2 :overflags))))
          (1
           (when field
             (setf (wp-numref p) (%w-bit br))
             (when (zerop (wp-numref p)) (setf (wp-reffield p) (%w-bit br))))
           (when (= 1 (ws-extended-mv s)) (setf (wp-mvrange p) (%w-unary br 0 3)))
           (when (= 1 (ws-interlace s))
             (when (= 1 (ws-extended-dmv s)) (setf (wp-dmvrange p) (%w-unary br 0 3)))
             (when (= 1 fcm)
               (setf (wp-fourmvswitch p) (%w-bit br) (wp-intcomp p) (%w-bit br))
               (when (= 1 (wp-intcomp p)) (setf (wp-lumscale p) (%w-bits br 6) (wp-lumshift p) (%w-bits br 6)))
               (plane 1 :skip)
               (setf (wp-mbmodetab p) (%w-bits br 2) (wp-imvtab p) (%w-bits br 2)
                     (wp-icbptab p) (%w-bits br 3) (wp-twomvbptab p) (%w-bits br 2))
               (when (= 1 (wp-fourmvswitch p)) (setf (wp-fourmvbptab p) (%w-bits br 2)))))
           (unless (= 1 fcm)
             (setf (wp-mv-mode p) (aref (aref +w-pmode+ low) (%w-unary br 1 4)))
             (when (= 4 (wp-mv-mode p))
               (setf (wp-mv-mode2 p) (aref (aref +w-pmode2+ low) (%w-unary br 1 3))
                     (wp-intcompfield p) (if field (logxor (%w-210 br) 3) 3))
               (when (logbitp 0 (wp-intcompfield p))
                 (setf (wp-lumscale p) (%w-bits br 6) (wp-lumshift p) (%w-bits br 6)))
               (when (and field (logbitp 1 (wp-intcompfield p)))
                 (setf (wp-lumscale2 p) (%w-bits br 6) (wp-lumshift2 p) (%w-bits br 6)))))
           (let ((mixed (or (= 3 (wp-mv-mode p)) (and (= 4 (wp-mv-mode p)) (= 3 (wp-mv-mode2 p))))))
             (case fcm
               (0 (when mixed (plane 2 :mvtype))
                  (plane 1 :skip)
                  (setf (wp-mvtab p) (%w-bits br 2) (wp-cbptab p) (%w-bits br 2)))
               (2 (setf (wp-mbmodetab p) (%w-bits br 3) (wp-imvtab p) (%w-bits br (+ 2 (wp-numref p)))
                        (wp-icbptab p) (%w-bits br 3))
                  (when mixed (setf (wp-fourmvbptab p) (%w-bits br 2))))))
           (when (plusp (ws-dquant s)) (%w-vopdquant br s p))
           (%w-transform br s p))
          (2
           (when (= 1 fcm)
             (when (zerop (aref +w-bfraction+ (bfraction))) (error "t420 video: a frame-interlaced BI by BFRACTION")))
           (when (= 1 (ws-extended-mv s)) (setf (wp-mvrange p) (%w-unary br 0 3)))
           (case fcm
             (2 (when (= 1 (ws-extended-dmv s)) (setf (wp-dmvrange p) (%w-unary br 0 3)))
                (setf (wp-mv-mode p) (aref (aref +w-pmode2+ low) (%w-unary br 1 3)))
                (plane 2 :forward)
                (setf (wp-mbmodetab p) (%w-bits br 3) (wp-imvtab p) (%w-bits br 3) (wp-icbptab p) (%w-bits br 3))
                (when (= 3 (wp-mv-mode p)) (setf (wp-fourmvbptab p) (%w-bits br 2)))
                (setf (wp-numref p) 1))
             (1 (when (= 1 (ws-extended-dmv s)) (setf (wp-dmvrange p) (%w-unary br 0 3)))
                (%w-bit br)                                 ; INTCOMP, always 0 in a B frame
                (setf (wp-mv-mode p) 1)
                (plane 0 :direct)
                (plane 1 :skip)
                (setf (wp-mbmodetab p) (%w-bits br 2) (wp-imvtab p) (%w-bits br 2) (wp-icbptab p) (%w-bits br 3)
                      (wp-twomvbptab p) (%w-bits br 2) (wp-fourmvbptab p) (%w-bits br 2)))
             (t (setf (wp-mv-mode p) (if (= 1 (%w-bit br)) 1 0))
                (plane 0 :direct)
                (plane 1 :skip)
                (setf (wp-mvtab p) (%w-bits br 2) (wp-cbptab p) (%w-bits br 2))))
           (when (plusp (ws-dquant s)) (%w-vopdquant br s p))
           (%w-transform br s p))))
      (%w-tables br s p)
      (setf (wp-bits p) (- (%w-pos br) start))
      p)))

;;; ---- the decoder ------------------------------------------------------------------------------------

(defstruct (t4w-dec (:conc-name t4wd-))
  (seq (make-w-seq)) pic cur last next
  (slices '())                  ; (offset size mb-bit-offset row), in order
  (fill 0) (width 0) (height 0) (mbw 0) (mbh 0)
  (rnd 0) (bfrac 0)
  (meta (make-hash-table :test #'eq))   ; surface -> (type ic lumscale lumshift top-type bottom-type)
  (first nil)                   ; a field pair's first field, once decoded
  (last-refdist 0)              ; the last anchor field's REFDIST (a B field's distances use it)
  (dmv2 (make-hash-table :test #'eq))   ; surface -> its bottom field's direct-MV buffer
  (shown 0) fn)


(defvar *t4w-n* 0 "Pictures handed to the engine (a field each), for naming the one that hangs.")
(defvar *t4w-bitplane* nil "(gpu . cpu): the packed bitplanes, mapped once.")
(defvar *t4w-rows* '() "(mbw . (intra deblock bsd)) row stores for VC-1.")

(defun %t4w-rows (mbw)
  (or (cdr (assoc mbw *t4w-rows*))
      (let ((r (list (t4g-alloc (* mbw 64) :map nil) (t4g-alloc (* mbw 7 64) :map nil)
                     (t4g-alloc (* mbw 96) :map nil))))
        (push (cons mbw r) *t4w-rows*) r)))

(defun %t4w-put (d bytes start end)
  (unless *t4v-bs*
    (multiple-value-bind (g c) (t4g-alloc +t4v-bitstream-bytes+) (setq *t4v-bs* (cons g c))))
  (let ((off (* 64 (ceiling (t4wd-fill d) 64))) (cpu (cdr *t4v-bs*)))
    (when (> (+ off (- end start) 64) +t4v-bitstream-bytes+) (error "t420 video: slice data overflow"))
    (loop for i from start below end by 4
          for o from off by 4
          do (t4g-w (+ cpu o) (logior (aref bytes i)
                                      (if (< (+ i 1) end) (ash (aref bytes (+ i 1)) 8) 0)
                                      (if (< (+ i 2) end) (ash (aref bytes (+ i 2)) 16) 0)
                                      (if (< (+ i 3) end) (ash (aref bytes (+ i 3)) 24) 0))))
    (setf (t4wd-fill d) (+ off (- end start)))
    off))

(defun %t4w-unescape (bytes start end)
  "BYTES[START..END) without its emulation-prevention bytes (an 03 after 00 00, before 00-03)."
  (let ((out (make-array (- end start) :element-type '(unsigned-byte 8))) (n 0) (zeros 0))
    (loop for i from start below end
          do (let ((b (aref bytes i)))
               (if (and (>= zeros 2) (= b 3) (< (1+ i) end) (< (aref bytes (1+ i)) 4))
                   (setq zeros 0)
                   (progn (setf (aref out n) b) (incf n)
                          (setq zeros (if (zerop b) (1+ zeros) 0))))))
    (subseq out 0 n)))

(defun %t4w-raw-bits (bytes start bits)
  "Bit offset BITS of the unescaped data from START, as an offset in the escaped bytes."
  (let ((i start) (k 0) (zeros 0) (want (ash bits -3)))
    (loop
      (let* ((b (aref bytes i))
             (epb (and (>= zeros 2) (= b 3) (< (1+ i) (length bytes)) (< (aref bytes (1+ i)) 4))))
        (when (and (>= k want) (not epb)) (return))
        (if epb (setq zeros 0) (progn (setq zeros (if (zerop b) (1+ zeros) 0)) (incf k)))
        (incf i)))
    (+ (* 8 (- i start)) (logand bits 7))))

(defun %t4w-show (d s)
  (when s (funcall (t4wd-fn d) d (t4wd-shown d) s) (incf (t4wd-shown d))))

(defun %t4w-free (d s)
  (when s (remhash s (t4wd-meta d)) (push s *t4v-pool*)))

(defun %t4w-bitplanes (d p)
  "Pack P's decoded planes into the bitplane buffer: a nibble a macroblock (even ones low), rows
   of ceil(mbw/2) bytes.  Its GPU address."
  (unless *t4w-bitplane*
    (multiple-value-bind (g c) (t4g-alloc 8192) (setq *t4w-bitplane* (cons g c))))
  (let* ((mbw (t4wd-mbw d)) (mbh (if (= 2 (wp-fcm p)) (floor (t4wd-mbh d) 2) (t4wd-mbh d))) (bw (ceiling mbw 2))
         (img (make-array (* 4 (ceiling (* bw mbh) 4)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y mbh)
      (dotimes (x mbw)
        (let ((v (if (= 4 (wp-type p)) 2
                     (let ((n 0))
                       (dolist (bp (wp-planes p) n)
                         (setq n (logior n (ash (aref (cdr bp) (+ (* y mbw) x)) (car bp))))))))
              (i (+ (* y bw) (ash x -1))))
          (setf (aref img i) (logior (aref img i) (ash v (if (oddp x) 4 0)))))))
    (loop for i from 0 below (length img) by 4
          do (t4g-w (+ (cdr *t4w-bitplane*) i)
                    (logior (aref img i) (ash (aref img (+ i 1)) 8)
                            (ash (aref img (+ i 2)) 16) (ash (aref img (+ i 3)) 24))))
    (car *t4w-bitplane*)))

(defparameter +w-bscale+
  #(128 85 170 64 192 51 102 153 204 43 215 37 74 111 148 185 222 32 96 160 224))

(defun %t4w-dquant (dquant p)
  "PIC_STATE's alternative quantiser: (values ALTPQ config edge-mask) for DQUANT and picture P."
  (let ((altpq (wp-altpq p)) (altcfg 0) (edge 0))
    (case dquant
      (0) (2 (setq altcfg 1 edge #xF))
      (1 (if (zerop (wp-dquantfrm p))
             (setq altpq 0)
             (progn (setq altcfg 1)
                    (case (wp-dqprofile p)
                      (3 (setq altcfg (if (zerop (wp-dqbilevel p)) 2 3) edge 0))
                      (0 (setq edge #xF))
                      (1 (setq edge (if (= 3 (wp-dqsbedge p)) 9 (ash 3 (wp-dqsbedge p)))))
                      (2 (setq edge (ash 1 (wp-dqsbedge p)))))))))
    (values altpq altcfg edge)))

(defun %t4w-meta (d p cur fwd type fcm second bottom)
  "Record what later pictures need to know of CUR, the picture P decodes into."
  ;; intensity compensation belongs to the forward reference (gen6_mfd_vc1_decode_init); a
  ;; field pair keeps each field's picture type; and whether the picture was coded interlaced
  (let* ((ty (if (= type 4) 1 type)) (old (and second (gethash cur (t4wd-meta d)))))
    (setf (gethash cur (t4wd-meta d))
          (list ty 0 0 0
                (if (and (= 2 fcm) bottom) (if old (fifth old) ty) ty)
                (if (and (= 2 fcm) (not bottom)) (if old (sixth old) ty) ty)
                fcm)))
  (when (and (= type 1) (= 4 (wp-mv-mode p)))
    (let ((m (copy-list (gethash fwd (t4wd-meta d)))))
      (setf (second m) 1 (third m) (wp-lumscale p) (fourth m) (wp-lumshift p)
            (gethash fwd (t4wd-meta d)) m))))

(defun %t4w-decode (d)
  "The open picture, on the engine."
  (let* ((vcs (t4v-start)) (ring (%t4v-batch)) (s (t4wd-seq d)) (p (t4wd-pic d))
         (cur (t4wd-cur d)) (mbw (t4wd-mbw d)) (mbh (t4wd-mbh d)) (rows (%t4w-rows mbw))
         (type (wp-type p)) (adv (= 3 (ws-profile s)))
         (fcm (wp-fcm p)) (field (= 2 fcm)) (second (wp-second p)) (tff (wp-tff p))
         (bottom (and field (if (= 1 tff) second (not second))))
         (mbh-e (if field (floor mbh 2) mbh))          ; a field's own height, as gen7 has it
         (fwd (case type ((1 4) (t4wd-next d)) (2 (t4wd-last d))))
         (bwd (if (= type 2) (t4wd-next d) nil))
         (dmv-of (lambda (surf bot)
                   (if bot
                       (or (gethash surf (t4wd-dmv2 d))
                           (setf (gethash surf (t4wd-dmv2 d)) (t4g-alloc (* 128 (t4wd-mbh d) 64) :map nil)))
                       (t4vs-dmv surf))))
         (lf (if (= type 4) 0 (ws-loopfilter s)))
         (planes-p (or (= type 4) (plusp (length (wp-planes p)))))
         (raw (wp-raw p))
         (mode (if (= 4 (wp-mv-mode p)) (wp-mv-mode2 p) (wp-mv-mode p)))
         (pq (wp-pq p)) (pred-pipe nil))
    (when (and (member type '(1 2 4)) (null fwd) (not (and second (= type 1))))
      (error "t420 video: a VC-1 P or B picture with no reference"))
    (%t4w-meta d p cur fwd type fcm second bottom)
    (flet ((out (&rest ds) (t4g-emit ring ds))
           (g (x) (if x (t4vs-gpu x) 0))
           (rawf (k) (if (member k raw) 1 0)))
      (out (ash #x26 23) 0 0 0)
      (out (logior +mfx-pipe-mode+ 2) (logior (if (= lf 1) (ash 1 9) (ash 1 8)) 1) (logior (ash 1 6) 1) 0)
      (out (logior +mfx-surface+ 4) 0
           (logior (ash (- (t4wd-height d) 1) 19) (ash (- (t4wd-width d) 1) 6))
           (logior (ash 4 28) (ash 1 27) (ash (- (t4vs-pitch cur) 1) 3) (ash 1 1) 1)
           (t4vs-h32 cur) 0)
      ;; frame-store slots (intel_update_vc1_frame_store_index): 0 and 2 forward, 1 and 3 backward
      ;; (or forward); a second field's own first field in slot 0 (top first) or 2
      ;; with no backward reference, 1 and 3 copy 0 and 2 after that
      (let* ((refs (list fwd bwd fwd bwd)))
        (when (and field second) (setf (nth (if (= 1 tff) 0 2) refs) cur))
        (unless bwd (setf (second refs) (first refs) (fourth refs) (third refs)))
        (apply #'out (logior +mfx-pipe-buf+ 22)
               (if (= lf 1) 0 (g cur)) (if (= lf 1) (g cur) 0) 0 0 (first rows) (second rows)
               (append (loop for i below 16 collect (g (nth (mod i 4) refs))) (list 0))))
      (out (logior +mfx-bsp-buf+ 2) (third rows) 0 (if planes-p (%t4w-bitplanes d p) 0))
      ;; VC1_PRED_PIPE_STATE
      (let* ((interp (if (= 1 fcm) 1 (case mode (0 2) (2 0) (t 1))))
             (fm (and fwd (member type '(1 2)) (gethash fwd (t4wd-meta d))))
             (ic (if fm (second fm) 0))
             ;; bits 10 and 9: the forward and the backward reference were coded interlaced, so
             ;; pixels beyond their edges repeat field by field (FFmpeg's last_interlaced and
             ;; next_interlaced); 11 and 8 for an interlaced picture (11: a second field's own
             ;; first field, the same).  Found by probing SA10143: gen6_mfd.c leaves bit 8 a
             ;; FIXME, and without 9-11 the top rows came out +-1-2 off
             (ilaced (lambda (r) (if (and r (plusp (or (seventh (gethash r (t4wd-meta d))) 0))) 1 0))))
        (setq pred-pipe (list (logior +mfx-vc1-pred+ 5)
             (logior (ash (funcall ilaced fwd) 10) (ash (funcall ilaced bwd) 9)
                     (if (plusp fcm) (logior (ash 1 11) (ash 1 8)) 0) (ash (wp-rnd p) 4) (ash (if adv 2 (ws-profile s)) 2))
             (if (= ic 1) (logior (ash (fourth fm) 16) (third fm)) 0)
             0 0 0
             (logior (ash interp 19) (ash (ws-fastuvmc s) 18) (ash (wp-rangeredfrm p) 16) (ash ic 2)))))
      ;; the profile in PRED_PIPE_STATE reaches intra reconstruction only from the NEXT picture's
      ;; PIC_STATE on (measured: after an Advanced stream, a Main I picture came out 128 too bright,
      ;; and right when decoded again).  So PRED_PIPE_STATE goes before PIC_STATE as well as after.
      (apply #'out pred-pipe)
      ;; VC1_PIC_STATE
      (let* ((dquant (ws-dquant s)) (altcfg 0) (edge 0) (altpq (wp-altpq p))
             (unified (if (= 1 fcm) 0 (case mode (0 3) (1 1) (2 2) (t 0))))
             (scale (if (< (t4wd-bfrac d) 21) (aref +w-bscale+ (t4wd-bfrac d)) 0))
             (ptype (if (= type 4) 1 type))
             (tt-flag (wp-ttmbf p)) (tt-frm (wp-ttfrm p))
             (ac-y (if (member type '(0 3)) (wp-ac-y p) (wp-ac-c p)))
             (bwd-meta (and bwd (gethash bwd (t4wd-meta d))))
             (dmv-valid (if (and (= type 2) bwd-meta (= 1 (if bottom (sixth bwd-meta) (fifth bwd-meta)))) 1 0))
             ;; a one-reference P field names the field it predicts from (gen7_mfd_vc1_pic_state)
             (polarity (if (and field (= type 1) (zerop (wp-numref p)))
                           (let ((first-p (not second)) (tf (= 1 tff)))
                             (if (zerop (wp-reffield p))
                                 (if (if first-p tf (not tf)) 1 0)
                                 (if (if first-p (not tf) tf) 1 0)))
                           0))
             (fcm-bits (case fcm (0 0) (1 1) (t (if (= 1 tff) 2 3))))
             (overlap 0))
        (multiple-value-setq (altpq altcfg edge) (%t4w-dquant dquant p))
        (when (and (member type '(0 3)) adv) (setq ptype 3))
        ;; a field names the PAIR's type, FPTYPE (I/I 0 .. BI/BI 7), as gen7's ptype does; BI for an
        ;; I field hangs the engine
        (when field (setq ptype (wp-fptype p)))
        (when (and (not (member type '(0 3))) (zerop (ws-vstransform s))) (setq tt-flag 1 tt-frm 0))
        (when (= 1 (ws-overlap s))
          (if adv
              (progn (when (and (= type 1) (>= pq 9)) (setq overlap 1))
                     (when (and (member type '(0 3)) (or (>= pq 9) (member (wp-condover p) '(1 2))))
                       (setq overlap 1)))
              (when (and (>= pq 9) (/= type 2)) (setq overlap 1))))
        (out (logior +mfx-vc1-pic+ 4)
             (logior (ash mbh-e 16) mbw)
             (logior (ash (ws-syncmarker s) 31) (ash 1 29) (ash altpq 24) (ash lf 23) (ash overlap 22)
                     (ash (if (zerop (ws-quantizer s)) 1 0) 21) (ash pq 16) (ash edge 12) (ash altcfg 10)
                     (ash (wp-halfqp p) 9) (ash (wp-pquantizer p) 8)
                     (ash (aref #(0 2 3) (wp-condover p)) 6) (if second (ash 1 5) 0) (ash ptype 2) fcm-bits)
             (logior (ash (if planes-p 1 0) 23) (ash (rawf :forward) 22) (ash (rawf :mvtype) 21) (ash (rawf :skip) 20)
                     (ash (rawf :direct) 19) (ash (rawf :overflags) 18) (ash (rawf :acpred) 17) (ash (rawf :fieldtx) 16)
                     (ash (wp-dmvrange p) 14) (ash (wp-mvrange p) 12) (ash (wp-fourmvswitch p) 11)
                     (ash (ws-fastuvmc s) 10) (ash unified 8)
                     (ash polarity 6) (ash (wp-numref p) 5) (logand (if (and field (= type 2)) (wp-frfd p) (wp-refdist p)) 31))
             (logior (ash scale 24) (ash (if (zerop fcm) (wp-mvtab p) (wp-imvtab p)) 20)
                     (ash (wp-fourmvbptab p) 18) (ash (wp-twomvbptab p) 16)
                     (ash tt-frm 12) (ash tt-flag 11) (ash (wp-mbmodetab p) 8)
                     (ash ac-y 6) (ash (wp-ac-c p) 4) (ash (wp-dctab p) 3)
                     (if (zerop fcm) (wp-cbptab p) (wp-icbptab p)))
             (logior (ash dmv-valid 13) (ash (if field (wp-brfd p) 0) 8) (- (floor (1+ mbw) 2) 1))))
      (apply #'out pred-pipe)
      ;; VC1_DIRECTMODE_STATE: a P picture writes its motion vectors, a B reads its backward one's
      (out (logior +mfx-vc1-direct+ 1) (if (member type '(1 4)) (funcall dmv-of cur bottom) 0)
           (if (and (= type 2) bwd) (funcall dmv-of bwd bottom) 0))
      (out (logior +mfx-ind-obj+ 9) (car *t4v-bs*) 0 0 0 0 0 0 0 0 0)
      (loop for (sl . rest) on (t4wd-slices d)
            do (destructuring-bind (off size mbo row) sl
                 (out (logior +mfd-vc1-bsd+ 2)
                      (- size (ash mbo -3)) (+ off (ash mbo -3))
                      (logior (ash row 24) (ash (if rest (fourth (first rest)) mbh-e) 16) (logand mbo 7)))))
      (out (ash #x0A 23) 0)
      (t4g-emit vcs (list (ash #x31 23) (t4r-gpu ring)))
      (let ((sq (t4g-breadcrumb vcs)))
        (t4v-submit vcs)
        (incf *t4w-n*)
        (unless (t4g-wait vcs sq 500)
          (let ((head (t4-gpu-reg #x12034)) (tail (t4-gpu-reg #x12030)))
            (t4v-reset)
            (error "t420 video: VC-1 picture ~D hung the engine (~A fcm ~D~:[~; second~]; head ~X tail ~X)"
                   (1- *t4w-n*) (char "IPBIS" type) fcm second head tail)))))))

(defun %t4w-start-picture (d p)
  "A new picture (or a field pair's first field) P: finish the open one, size the surfaces, take one."
  (%t4w-end-picture d)
  (let ((s (t4wd-seq d)))
    (setf (t4wd-width d) (ws-width s) (t4wd-height d) (ws-height s)
          (t4wd-mbw d) (ceiling (ws-width s) 16) (t4wd-mbh d) (ceiling (ws-height s) 16)
          (t4wd-pic d) p (t4wd-rnd d) (wp-rnd p) (t4wd-first d) nil
          (t4wd-cur d) (t4v-surface-new (t4wd-mbw d) (t4wd-mbh d)))
    (when (= 2 (wp-type p)) (setf (t4wd-bfrac d) (wp-bfrac p)))
    (when (and (= 2 (wp-fcm p)) (member (wp-type p) '(0 1)))
      (setf (t4wd-last-refdist d) (wp-refdist p)))))

(defun %t4w-second-field (d p)
  "A field start code: decode the pair's first field, and make P (the second's header) the open one."
  (%t4w-decode d)
  (setf (t4wd-first d) (t4wd-pic d) (t4wd-pic d) p (t4wd-slices d) '() (t4wd-fill d) 0)
  (when (= 2 (wp-type p)) (setf (t4wd-bfrac d) (wp-bfrac p))))

(defun %t4w-end-picture (d)
  (let ((p (t4wd-pic d)) (cur (t4wd-cur d)))
    (when (and p cur)
      (if (= 4 (wp-type p))
          (progn                                            ; skipped: the reference again
            (t4g-wait *t4g-blt* (progn (t4g-blt-copy (t4vs-gpu cur) 4096 0 0 (t4vs-gpu (t4wd-next d)) 4096 0 0 1024
                                                     (ceiling (* (t4vs-pitch cur) (+ (t4vs-h32 cur) (* 32 (ceiling (ash (t4vs-h cur) -1) 32)))) 4096))
                                       (let ((sq (t4g-breadcrumb *t4g-blt*))) (t4g-submit *t4g-blt*) sq))
                      500)
            (setf (gethash cur (t4wd-meta d))
                  (list 1 0 0 0 1 1 (or (seventh (gethash (t4wd-next d) (t4wd-meta d))) 0))))
          (%t4w-decode d))
      ;; a field pair is shown and retired as a frame, by its first field's type
      (if (member (wp-type (or (t4wd-first d) p)) '(2 3))
          (progn (%t4w-show d cur) (%t4w-free d cur))
          (progn (%t4w-show d (t4wd-next d))
                 (%t4w-free d (t4wd-last d))
                 (setf (t4wd-last d) (t4wd-next d) (t4wd-next d) cur)))))
  (setf (t4wd-pic d) nil (t4wd-cur d) nil (t4wd-first d) nil (t4wd-slices d) '() (t4wd-fill d) 0))

(defun %t4w-finish (d)
  (%t4w-end-picture d)
  (%t4w-show d (t4wd-next d))
  (%t4w-free d (t4wd-last d)) (%t4w-free d (t4wd-next d))
  (t4wd-shown d))

(defun t4w-vc1-p (bytes)
  "An Advanced-profile VC-1 elementary stream (it opens with a sequence header, 00 00 01 0F)?"
  (eql #x0F (%t4v-first-code bytes)))

(defun t4w-rcv-p (bytes)
  "An RCV file (Simple/Main VC-1: 0xC5 in its fourth byte, then struct C's length, 4 or more)?"
  (and (> (length bytes) 36) (= #xC5 (aref bytes 3)) (<= 4 (aref bytes 4) 16) (zerop (aref bytes 5))))

(defun %t4w-decode-advanced (d bytes)
  (let ((i (reel.mpeg2::find-start-code bytes 0)) (end (length bytes)) (s (t4wd-seq d)))
    (loop while (and i (< (+ i 4) end))
          do (let* ((code (aref bytes (+ i 3)))
                    (next (or (reel.mpeg2::find-start-code bytes (+ i 4)) end))
                    (payload (+ i 4)))
               (flet ((br () (let ((u (%t4w-unescape bytes payload next)))
                               (reel.mpeg2::make-br u))))
                 (case code
                   (#x0F (%t4w-end-picture d) (%w-seq-advanced (br) s))
                   (#x0E (%t4w-end-picture d) (%w-entry-point (br) s))
                   (#x0D (let* ((mbw (ceiling (ws-width s) 16)) (mbh (ceiling (ws-height s) 16))
                                (p (%w-picture-advanced (br) s mbw mbh nil (t4wd-last-refdist d))))
                           (%t4w-start-picture d p)
                           (unless (= 4 (wp-type p))
                             (push (list (%t4w-put d bytes payload next) (- next payload)
                                         (%t4w-raw-bits bytes payload (wp-bits p)) 0)
                                   (t4wd-slices d)))))
                   (#x0B (let* ((b (br)) (row (%w-bits b 9)))
                           (when (= 1 (%w-bit b))
                             (%w-picture-advanced b s (t4wd-mbw d) (t4wd-mbh d)))
                           (setf (t4wd-slices d)
                                 (append (t4wd-slices d)
                                         (list (list (%t4w-put d bytes payload next) (- next payload)
                                                     (%t4w-raw-bits bytes payload (%w-pos b)) row))))))
                   (#x0C (let* ((b (br))
                                (p (%w-picture-advanced b s (t4wd-mbw d) (t4wd-mbh d) (t4wd-pic d))))
                           (%t4w-second-field d p)
                           (when (member (wp-type p) '(0 1)) (setf (t4wd-last-refdist d) (wp-refdist p)))
                           (push (list (%t4w-put d bytes payload next) (- next payload)
                                       (%t4w-raw-bits bytes payload (wp-bits p)) 0)
                                 (t4wd-slices d))))
                   (#x0A (%t4w-end-picture d))))
               (setq i (if (< next end) next nil))))))

(defun %t4w-decode-rcv (d bytes)
  (let* ((s (t4wd-seq d)) (clen (logior (aref bytes 4) (ash (aref bytes 5) 8)
                                        (ash (aref bytes 6) 16) (ash (aref bytes 7) 24)))
         (at (+ 8 clen)))
    (%w-seq-simple-main (reel.mpeg2::make-br bytes :start 8 :end (+ 8 clen)) s)
    (flet ((u32 (i) (logior (aref bytes i) (ash (aref bytes (+ i 1)) 8)
                            (ash (aref bytes (+ i 2)) 16) (ash (aref bytes (+ i 3)) 24))))
      (setf (ws-height s) (u32 at) (ws-width s) (u32 (+ at 4)))
      (setq at (+ at 8 4 (u32 (+ at 8))))                  ; height, width, struct B's length and body
      (loop while (< (+ at 8) (length bytes))
            do (let* ((size (logand (u32 at) #xFFFFFF)) (start (+ at 8)) (end (+ start size)))
                 (when (> end (length bytes)) (return))
                 (if (<= size 1)
                     (progn (%t4w-start-picture d (make-w-pic :type 4)))
                     (let* ((mbw (ceiling (ws-width s) 16)) (mbh (ceiling (ws-height s) 16))
                            (p (%w-picture-simple-main (reel.mpeg2::make-br bytes :start start :end end)
                                                       s mbw mbh (t4wd-rnd d))))
                       (%t4w-start-picture d p)
                       (push (list (%t4w-put d bytes start end) size (wp-bits p) 0) (t4wd-slices d))))
                 (setq at end))))))

(defun t4w-decode-bytes (bytes &optional (fn (lambda (d i s) (declare (ignore d i s)))))
  "Decode VC-1 BYTES (an Advanced elementary stream or an RCV file) on the engine; FN gets
   (decoder index surface) for each picture in display order.  The count."
  (let ((d (make-t4w-dec :fn fn)))
    (cond ((t4w-vc1-p bytes) (%t4w-decode-advanced d bytes))
          ((t4w-rcv-p bytes) (%t4w-decode-rcv d bytes))
          (t (error "t420 video: not a VC-1 stream this knows")))
    (%t4w-finish d)))
