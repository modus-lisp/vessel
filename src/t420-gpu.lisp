;;;; t420-gpu.lisp -- the T420's GPU (Intel HD 3000, Sandy Bridge, gen 6), driven directly.
;;;;
;;;; STAGES (the plan): 1 the display engine's own planes -- the hardware cursor, tear-free
;;;; updates; 2 the blitter (BCS) through a command ring; 3 the render engine.  Stage 2's command
;;;; submission -- rings, batches, breadcrumbs, GPU memory through the GTT, forcewake -- is built to
;;;; carry stage 3.
;;;;
;;;; GPU MEMORY.  The BIOS maps graphics stolen memory (BDSM, 0xDBA00000 on this machine)
;;;; linearly through the GTT: aperture page N (BAR2 + N * 4 KB) is stolen page N.  The desk's
;;;; framebuffer is surface 0 (pages 0 .. 1031 at 1366x768, 5504 bytes a row).  Everything else
;;;; this file puts in GPU memory is laid out past it, from GPU address #x500000 (*T4G-POOL*).
;;;; The CPU reaches a GPU page through the aperture, mapped uncached in the MMIO window.
;;;;
;;;; Registers go through T4-GPU-REG / T4-GPU-SET (t420.lisp: the first 1 MB of BAR0).  Loaded
;;;; after app.lisp (*KILN-SHOW-MOVE*); what t420-desk.lisp defines, later, is called by name.

;;; ---- GPU memory, from the CPU -------------------------------------------------------------------

(defvar *t4g-aperture* nil "BAR2's physical base (the aperture).")
(defparameter *t4g-pool* #x500000 "GPU address of the first page this file allocates.")
(defvar *t4g-next* nil "The next free GPU address in the pool.")

(defun %t4g-aperture ()
  (or *t4g-aperture*
      (setq *t4g-aperture* (logand (modus-int::pci-config-read 0 2 0 #x18) (lognot #xF)))))

(defun t4g-alloc (bytes &key (align 4096) (map t))
  "BYTES of GPU memory from the pool: (values gpu-address cpu-address).  The CPU address is an
   uncached mapping of the aperture (one 32-bit access at a time: T4G-W / T4G-R), or NIL with MAP
   NIL -- for what only the GPU touches (a depth buffer, a back buffer): the MMIO window the
   mappings come from is 4 MB, and one 1366x768 surface is more."
  (unless *t4g-next* (setq *t4g-next* *t4g-pool*))
  (let* ((gpu (* align (ceiling *t4g-next* align)))
         (cpu (and map (modus-int::pc-mmio-map (+ (%t4g-aperture) gpu) bytes))))
    (setq *t4g-next* (+ gpu bytes))
    (values gpu cpu)))

(defun t4g-w (va v) (modus-int::%mmio-write32 va (logand v #xFFFFFFFF)))
(defun t4g-r (va) (modus-int::%mmio-read32 va))

;;; ---- stage 1: the hardware cursor -----------------------------------------------------------------
;;; Pipe A's cursor plane: CURACNTR (#x70080) mode #x27 = 64x64 ARGB, CURABASE (#x70084) its image's
;;; GPU address (4 KB aligned; the write that arms the update, so it goes last), CURAPOS (#x70088)
;;; x in bits 11:0 and y in 27:16, each with a sign bit above (15, 31).  The plane is blended over
;;; the primary by the display engine: moving the pointer is one register write, and the desk's
;;; pixels are never touched.

(defvar *t4g-cursor* nil "The cursor image's GPU address once the plane is on, else NIL.")

(defun %t4g-cursor-image (cpu)
  "The arrow (t420.lisp *T4-ARROW*) as 64x64 ARGB at CPU: 2 black, 1 white, else clear."
  (dotimes (y 64)
    (let ((line (and (< y (length *t4-arrow*)) (nth y *t4-arrow*))))
      (dotimes (x 64)
        (t4g-w (+ cpu (* 4 (+ (* y 64) x)))
               (let ((c (and line (< x (length line)) (char line x))))
                 (case c (#\2 #xFF000000) (#\1 #xFFFFFFFF) (t 0))))))))

(defun t4g-cursor-move (x y)
  "Put the hardware cursor's hot spot (its top left) at screen (X, Y)."
  (when *t4g-cursor*
    (t4-gpu-set #x70088 (logior (if (minusp y) (ash 1 31) 0) (ash (logand (abs y) #xFFF) 16)
                                (if (minusp x) (ash 1 15) 0) (logand (abs x) #xFFF)))
    (t4-gpu-set #x70084 *t4g-cursor*)))                 ; the base write latches the position

(defun t4g-cursor-on (x y)
  "Turn the hardware cursor on at (X, Y).  Its GPU address."
  (unless *t4g-cursor*
    (multiple-value-bind (gpu cpu) (t4g-alloc (* 64 64 4))
      (%t4g-cursor-image cpu)
      (setq *t4g-cursor* gpu)))
  (t4-gpu-set #x70080 #x27)
  (t4g-cursor-move x y)
  *t4g-cursor*)

(defun t4g-cursor-off ()
  (t4-gpu-set #x70080 0)
  (when *t4g-cursor* (t4-gpu-set #x70084 *t4g-cursor*)))

;;; ---- the GT: forcewake ---------------------------------------------------------------------------
;;; Sandy Bridge power-gates the GT (render, blitter, video) apart from the display: until something
;;; holds it awake its registers read 0 (measured: every ring register, even MI_MODE).  FORCEWAKE
;;; (#xA18C) <- 1 asks; FORCEWAKE_ACK (#x130090, past the 1 MB T4-GPU-REG maps) bit 0 answers.
;;; Held for good: a driver that lets the GT sleep (RC6) must take it around every GT access.

(defvar *t4g-hi* nil "BAR0 + #x130000, mapped: FORCEWAKE_ACK and the GT's other high registers.")

(defun t4g-forcewake ()
  "Wake the GT and keep it awake.  T when it acknowledged."
  (unless *t4g-hi*
    (setq *t4g-hi* (modus-int::pc-mmio-map
                    (+ (logand (modus-int::pci-config-read 0 2 0 #x10) (lognot 15)) #x130000) #x1000)))
  (t4-gpu-set #xA18C 1)
  (dotimes (i 100 nil)
    (when (logbitp 0 (t4g-r (+ *t4g-hi* #x90))) (return t))
    (modus-int::%sleep-ms 1)))

;;; ---- command rings ---------------------------------------------------------------------------------
;;; A ring is a circular buffer of commands in GPU memory, with HEAD (where the engine reads) and
;;; TAIL (where we write; moving it submits).  Each engine has one at its register base -- the
;;; blitter's at #x22000, the render engine's at #x2000 (stage 3) -- and a hardware status page
;;; (HWS, gen 6: base + #x2080) the engine writes breadcrumbs into: MI_STORE_DWORD_INDEX puts a
;;; sequence number there once everything before it has run.

(defstruct (t4g-ring (:conc-name t4r-))
  base gpu cpu (size 16384) (tail 0) hws hws-cpu (seq 0) (name ""))

(defun t4g-ring-start (base name)
  "Program the engine at register BASE with a new ring and status page.  The ring, or NIL."
  (let ((r (make-t4g-ring :base base :name name)))
    (multiple-value-bind (g c) (t4g-alloc 4096) (setf (t4r-hws r) g (t4r-hws-cpu r) c))
    (multiple-value-bind (g c) (t4g-alloc (t4r-size r)) (setf (t4r-gpu r) g (t4r-cpu r) c))
    (dotimes (i 1024) (t4g-w (+ (t4r-hws-cpu r) (* 4 i)) 0))
    (dotimes (i (/ (t4r-size r) 4)) (t4g-w (+ (t4r-cpu r) (* 4 i)) 0))
    (flet ((reg (off) (+ base off)))
      (t4-gpu-set (reg #x3C) 0)                                   ; RING_CTL: stopped
      (t4-gpu-set (reg #x34) 0) (t4-gpu-set (reg #x30) 0)         ; HEAD, TAIL
      (t4-gpu-set (reg #x2080) (t4r-hws r))                       ; HWS_PGA (gen 6)
      (t4-gpu-set (reg #x38) (t4r-gpu r))                         ; RING_START
      (t4-gpu-set (reg #x34) 0) (t4-gpu-set (reg #x30) 0)
      (t4-gpu-set (reg #x3C) (logior (logand (- (t4r-size r) 4096) #x1FF000) 1))   ; size, valid
      (when (and (logbitp 0 (t4-gpu-reg (reg #x3C))) (= (t4-gpu-reg (reg #x38)) (t4r-gpu r)))
        r))))

(defun t4g-emit (r dwords)
  (dolist (d dwords)
    (t4g-w (+ (t4r-cpu r) (t4r-tail r)) d)
    (setf (t4r-tail r) (mod (+ (t4r-tail r) 4) (t4r-size r)))))

(defun t4g-breadcrumb (r)
  "Flush, then have the engine write the next sequence number into its status page.  The number."
  (let ((seq (incf (t4r-seq r))))
    (t4g-emit r (list (ash #x26 23) 0 0 0))                        ; MI_FLUSH_DW
    (t4g-emit r (list (logior (ash #x21 23) 1) (* 4 #x20) seq))   ; MI_STORE_DWORD_INDEX -> HWS[#x20]
    seq))

(defun t4g-submit (r)
  "Hand the engine everything emitted (the tail is kept qword aligned with MI_NOOPs)."
  (when (logtest (t4r-tail r) 4) (t4g-emit r (list 0)))
  (t4-gpu-set (+ (t4r-base r) #x30) (t4r-tail r)))

(defun t4g-seq-done (r) (t4g-r (+ (t4r-hws-cpu r) (* 4 #x20))))

(defun t4g-wait (r seq &optional (ms 50))
  "Wait until the engine has run past breadcrumb SEQ.  T, or NIL after MS."
  (let ((end (+ (get-internal-real-time) (* ms (floor internal-time-units-per-second 1000)))))
    (loop (when (>= (t4g-seq-done r) seq) (return t))
          (when (>= (get-internal-real-time) end) (return nil)))))

;;; ---- stage 2: the blitter ---------------------------------------------------------------------------
;;; XY_COLOR_BLT and XY_SRC_COPY_BLT at 32 bits a pixel (BR13 colour depth 3, both RGB and alpha
;;; written).  Surfaces are GPU addresses and pitches in bytes; the screen is surface 0, pitch
;;; *T4-PITCH*.  A copy within one surface may overlap: the engine orders it (measured).

(defvar *t4g-blt* nil "The blitter's ring once started.")

(defun t4g-blt-fill (dst pitch x y w h rgb)
  (t4g-emit *t4g-blt* (list (logior (ash 2 29) (ash #x50 22) (ash 3 20) 4)
                            (logior (ash 3 24) (ash #xF0 16) pitch)
                            (logior (ash y 16) x) (logior (ash (+ y h) 16) (+ x w)) dst rgb)))

(defun t4g-blt-copy (dst dpitch dx dy src spitch sx sy w h)
  (t4g-emit *t4g-blt* (list (logior (ash 2 29) (ash #x53 22) (ash 3 20) 6)
                            (logior (ash 3 24) (ash #xCC 16) dpitch)
                            (logior (ash dy 16) dx) (logior (ash (+ dy h) 16) (+ dx w)) dst
                            (logior (ash sy 16) sx) spitch src)))

(defun t4g-blitter-start ()
  "Wake the GT and start the blitter's ring.  The ring, or NIL."
  (when (t4g-forcewake)
    (setq *t4g-blt* (t4g-ring-start #x22000 "blitter"))))

;;; ---- a window move, on the blitter -------------------------------------------------------------------
;;; app.lisp's *KILN-SHOW-MOVE*: glass marked the frame an exact move, so the screen already holds
;;; the source pixels.  The blitter moves them where they now belong; the CPU copies only the strips
;;; the move uncovered.  A drag frame was the whole old+new box copied by the CPU.

(defvar *t4g-moves* 0 "Window moves shown by the blitter.")

(defun %t4g-box-minus (a b)
  "Box A (x y w h) less box B, as up to four boxes."
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (let ((x0 (max ax bx)) (y0 (max ay by)) (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
        (if (or (>= x0 x1) (>= y0 y1))
            (list a)
            (remove-if-not (lambda (r) (and (plusp (third r)) (plusp (fourth r))))
                           (list (list ax ay aw (- y0 ay)) (list ax y1 aw (- (+ ay ah) y1))
                                 (list ax y0 (- x0 ax) (- y1 y0)) (list x1 y0 (- (+ ax aw) x1) (- y1 y0)))))))))

(defun t4g-show-move (fb damage copy)
  "Show an exact move frame: COPY on the blitter, DAMAGE less its destination by the CPU.  T."
  (destructuring-bind (sx sy dx dy w h) copy
    (destructuring-bind (x0 y0 x1 y1) damage
      (let ((fw (glass:fb-width fb)) (fh (glass:fb-height fb)))
        (when (and *t4g-blt* (<= 0 sx) (<= 0 sy) (<= 0 dx) (<= 0 dy)
                   (<= (+ sx w) fw) (<= (+ dx w) fw) (<= (+ sy h) fh) (<= (+ dy h) fh)
                   (<= 0 x0) (<= 0 y0) (<= x1 fw) (<= y1 fh))
          (funcall '%t4-beam-clear y0 (- y1 y0))              ; tear-free, as a CPU blit (t420-desk.lisp)
          (t4g-blt-copy 0 *t4-pitch* dx dy 0 *t4-pitch* sx sy w h)
          (let ((seq (t4g-breadcrumb *t4g-blt*)))
            (t4g-submit *t4g-blt*)
            (unless (t4g-wait *t4g-blt* seq)
              ;; the engine did not answer: never again, and this frame the CPU's way, whole
              (t4-log "kiln t420: the blitter did not finish a copy; window moves go back to the CPU")
              (setf *kiln-show-move* nil)
              (funcall '%kiln-blit-region fb 1 0 0 x0 y0 (- x1 x0) (- y1 y0))
              (return-from t4g-show-move t)))
          (dolist (b (%t4g-box-minus (list x0 y0 (- x1 x0) (- y1 y0)) (list dx dy w h)))
            (apply '%kiln-blit-region fb 1 0 0 b))
          (incf *t4g-moves*)
          t)))))

;;; ---- stage 3: the render engine ----------------------------------------------------------------------
;;; The 3D pipeline, first drawn on the T420 2026-11-23: a 256x128 texture onto the screen through
;;; vertex fetch, a pass-through VS, setup (SF), the windower (WM), a pixel shader and the sampler --
;;; intel-gpu-tools' gen 6 render copy (lib/rendercopy_gen6.c), command for command.
;;;
;;; THE RING'S SETUP is Linux 5.4's for gen 6 (init_render_ring, gen6_init_clock_gating): MI_MODE,
;;; GFX_MODE, CACHE_MODE_0, INSTPM, the 3D chickens, GT_MODE's WIZ hashing, the unit clock gating.
;;; Masked registers: the high half says which bits the low half writes.
;;;
;;; PIPE_CONTROL ON GEN 6 needs its workarounds (Linux gen6_emit_post_sync_nonzero_flush): a CS stall
;;; at the scoreboard, then a post-sync write, before any flush with a post-sync operation; the
;;; writes go to a scratch page.  The breadcrumb is a flushing PIPE_CONTROL that writes the sequence
;;; number into the status page.
;;;
;;; A BATCH is one GPU page set: commands from 0, the state the commands point at from 1088
;;; (binding table, surface states, the kernel, the sampler, the CC viewport, the blend state --
;;; STATE_BASE_ADDRESS makes every pointer an offset into the batch), the RECTLIST's three vertices
;;; after the commands, and offset 1024 left zero for the depth-stencil and colour-calc states.  It
;;; is started from the ring with MI_BATCH_BUFFER_START through the global GTT (no PPGTT here, so
;;; not the "non-secure" form Linux uses).
;;;
;;; THE PIXEL SHADER is intel-gpu-tools' ps_kernel_nomask_affine (MIT licence, Intel): interpolate
;;; (u,v), sample, write the render target.  Writing our own kernels needs an EU assembler: next.

 ;; ---- the render engine (RCS, base #x2000): gen 6 setup, as Linux 5.4's init_render_ring and
 ;; gen6_init_clock_gating program it.  Masked registers: the high half says which bits to write.
 (defun %t4r-masked (on off) (logior (ash (logior on off) 16) on))
 (defun t4g-render-setup ()
   (t4-gpu-set #x209C (%t4r-masked (logior (ash 1 6) (ash 1 14)) 0))     ; MI_MODE: VS timer, no async-flip perf
   (t4-gpu-set #x2520 (%t4r-masked (ash 1 13) 0))                        ; GFX_MODE: explicit TLB invalidation
   (t4-gpu-set #x2120 (%t4r-masked 0 (logior (ash 1 5) 1)))              ; CACHE_MODE_0: no LRA, no RC op flush
   (t4-gpu-set #x20C0 (%t4r-masked (ash 1 7) 0))                         ; INSTPM: force ordering
   (t4-gpu-set #x2084 (%t4r-masked (ash 1 10) 0))                        ; _3D_CHICKEN: no HiZ with MSAA 4x
   (t4-gpu-set #x20D0 (logior (ash (logior (ash 1 9) (ash 1 7)) 16) (ash 1 9)))   ; GT_MODE: WIZ hashing 16x4
   (t4-gpu-set #x9400 (logior (t4-gpu-reg #x9400) (ash 1 5) (ash 1 7)))  ; UCGCTL1
   (t4-gpu-set #x9404 (logior (ash 1 12) (ash 1 11)))                    ; UCGCTL2
   (t4-gpu-set #x2090 (%t4r-masked (logior (ash 1 5) (ash 1 1)) 0)))     ; _3D_CHICKEN3
 (defvar *t4g-rcs* nil)
 (defvar *t4g-scratch* nil)
 (defun t4g-render-start ()
   (t4g-forcewake)
   (t4g-render-setup)
   (setq *t4g-scratch* (t4g-alloc 4096))
   (setq *t4g-rcs* (t4g-ring-start #x2000 "render")))
 ;; ---- PIPE_CONTROL, with the gen 6 workarounds (Linux: gen6_emit_post_sync_nonzero_flush)
 (defconstant +pc4+ #x7A000002)   ; GFX_OP_PIPE_CONTROL(4)
 (defun %t4r-pc (r flags addr data)
   (t4g-emit r (list +pc4+ flags addr data)))
 (defun %t4r-post-sync-nonzero (r)
   (%t4r-pc r (logior (ash 1 20) (ash 1 1)) 0 0)                          ; CS stall + stall at scoreboard
   (%t4r-pc r (ash 1 14) (logior *t4g-scratch* 4) 0))                     ; QW write to scratch (global GTT)
 (defun t4g-render-invalidate (r)
   (%t4r-post-sync-nonzero r)
   (%t4r-pc r (logior (ash 1 18) (ash 1 11) (ash 1 10) (ash 1 4) (ash 1 3) (ash 1 2) (ash 1 14) (ash 1 20))
            (logior *t4g-scratch* 4) 0))
 (defun t4g-render-breadcrumb (r)
   (let ((seq (incf (t4r-seq r))))
     (%t4r-post-sync-nonzero r)
     (%t4r-pc r (logior (ash 1 12) (ash 1 0) (ash 1 5) (ash 1 14) (ash 1 20))
              (logior (+ (t4r-hws r) (* 4 #x20)) 4) seq)
     seq))
 ;; ---- a batch: commands from 0, state from 1088, vertices after the commands
 (defvar *t4g-batch* nil) (defvar *t4g-batch-cpu* nil)
 (defparameter +t4b-size+ 8192)
 (defun %bw (off v) (t4g-w (+ *t4g-batch-cpu* off) v))
 (defparameter *t4g-ps-copy*        ; igt-gpu-tools lib/rendercopy_gen6.c ps_kernel_nomask_affine (MIT)
   '(#x0060005a #x204077be #x000000c0 #x008d0040  #x0060005a #x206077be #x000000c0 #x008d0080
     #x0060005a #x208077be #x000000d0 #x008d0040  #x0060005a #x20a077be #x000000d0 #x008d0080
     #x00000201 #x20080061 #x00000000 #x00000000  #x00600001 #x20200022 #x008d0000 #x00000000
     #x02800031 #x21c01cc9 #x00000020 #x0a8a0001  #x00600001 #x204003be #x008d01c0 #x00000000
     #x00600001 #x206003be #x008d01e0 #x00000000  #x00600001 #x208003be #x008d0200 #x00000000
     #x00600001 #x20a003be #x008d0220 #x00000000  #x00600001 #x20c003be #x008d0240 #x00000000
     #x00600001 #x20e003be #x008d0260 #x00000000  #x00600001 #x210003be #x008d0280 #x00000000
     #x00600001 #x212003be #x008d02a0 #x00000000  #x05800031 #x24001cc8 #x00000040 #x90019000))
 (defun %t4r-surface (off gpu w h pitch)
   (%bw off (logior (ash 1 29) (ash #x0C0 18) (ash 1 13)))   ; 2D, B8G8R8A8_UNORM, colour blend
   (%bw (+ off 4) gpu)
   (%bw (+ off 8) (logior (ash (- w 1) 6) (ash (- h 1) 19)))
   (%bw (+ off 12) (ash (- pitch 1) 3))
   (%bw (+ off 16) 0) (%bw (+ off 20) 0))
 (defun t4g-render-copy (src sw sh spitch dst dw dh dpitch dx dy)
   "SRC (SW x SH) onto DST at (DX,DY), through the 3D pipeline.  The breadcrumb."
   (unless *t4g-batch*
     (multiple-value-bind (g c) (t4g-alloc +t4b-size+) (setq *t4g-batch* g *t4g-batch-cpu* c)))
   (dotimes (i (/ +t4b-size+ 4)) (%bw (* 4 i) 0))
   (let* ((b *t4g-batch*) (bt 1088) (ss-dst 1120) (ss-src 1152) (kern 1216)
          (sampler (+ kern (* 4 (length *t4g-ps-copy*)))) (vp (+ sampler 32)) (blend 1728)
          (p 0))
     ;; state
     (%bw bt ss-dst) (%bw (+ bt 4) ss-src)
     (%t4r-surface ss-dst dst dw dh dpitch)
     (%t4r-surface ss-src src sw sh spitch)
     (loop for d in *t4g-ps-copy* for o from kern by 4 do (%bw o d))
     (%bw sampler (logior (ash 1 28) (ash 1 29))) (%bw (+ sampler 4) (logior 4 (ash 4 3) (ash 4 6)))
     (%bw vp #xF99A130C) (%bw (+ vp 4) #x799A130C)
     (%bw blend (logior #x11 (ash 1 5) (ash 1 31))) (%bw (+ blend 4) 3)
     ;; commands
     (flet ((out (&rest ds) (dolist (d ds) (%bw p d) (incf p 4))))
       (out #x69040000)                                           ; PIPELINE_SELECT 3D
       (out (logior #x790D0000 1) 0 0)                            ; 3DSTATE_MULTISAMPLE
       (out #x78180000 1)                                         ; 3DSTATE_SAMPLE_MASK
       (out (logior #x61010000 8) 0 (logior b 1) (logior b 1) 0 (logior b 1) 0 1 0 1)   ; STATE_BASE_ADDRESS
       (out #x61020000 0)                                         ; STATE_SIP
       (out (logior #x78050000 1) 24 0)                           ; 3DSTATE_URB: 24 VS entries
       (out (logior #x780D0000 (ash 1 12) 2) 0 0 vp)              ; VIEWPORT_STATE_POINTERS (CC)
       (out (logior #x78150000 3) 0 0 0 0)                        ; CONSTANT_VS
       (out (logior #x78100000 4) 0 0 0 0 0)                      ; 3DSTATE_VS: pass-through
       (out (logior #x78160000 3) 0 0 0 0)                        ; CONSTANT_GS
       (out (logior #x78110000 5) 0 0 0 0 0 0)                    ; 3DSTATE_GS
       (out (logior #x78120000 2) 0 0 0)                          ; 3DSTATE_CLIP
       (out (logior #x78170000 3) 0 0 0 0)                        ; CONSTANT_PS
       (out (logior #x79050000 5) (logior (ash 7 29) (ash 1 18)) 0 0 0 0 0)   ; null depth buffer
       (out #x79100000 0)                                         ; CLEAR_PARAMS
       (out (logior #x79000000 2) 0 (logior (ash (- dh 1) 16) (- dw 1)) 0)   ; DRAWING_RECTANGLE
       (out (logior #x780E0000 2) (logior blend 1) (logior 1024 1) (logior 1024 1))   ; CC_STATE_POINTERS
       (out (logior #x78020000 (ash 1 12) 2) 0 0 sampler)         ; SAMPLER_STATE_POINTERS (PS)
       (out (logior #x78130000 18) (logior (ash 1 22) (ash 1 11) (ash 1 4)) 0 (ash 1 29) (ash 2 25)
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)                          ; 3DSTATE_SF
       (out (logior #x78140000 7) kern (logior (ash 1 27) (ash 2 18)) 0 (ash 6 16)
            (logior (ash 39 25) (ash 1 19) (ash 1 1)) (logior (ash 1 20) (ash 1 10)) 0 0)   ; 3DSTATE_WM
       (out (logior #x78090000 5)                                 ; VERTEX_ELEMENTS: pad, x y, u v
            (logior (ash 1 25) (ash #x000 16)) (logior (ash 2 28) (ash 2 24) (ash 2 20) (ash 2 16))
            (logior (ash 1 25) (ash #x0F6 16)) (logior (ash 1 28) (ash 1 24) (ash 3 20) (ash 3 16))
            (logior (ash 1 25) (ash #x085 16) 4) (logior (ash 1 28) (ash 1 24) (ash 2 20) (ash 2 16)))
       (out (logior #x78010000 (ash 1 12) 2) 0 0 bt)              ; BINDING_TABLE_POINTERS (PS)
       (out (logior #x78080000 3) (logior 12) b (+ b +t4b-size+ -1) 0)   ; VERTEX_BUFFERS: pitch 12
       (let ((vstart (* 12 (ceiling (+ p 32) 12))))               ; after the primitive and BB_END
         (out (logior #x7B000000 (ash #x0F 10) 4) 3 (/ vstart 12) 1 0 0)   ; 3DPRIMITIVE RECTLIST
         (out (ash #x0A 23) 0)                                    ; MI_BATCH_BUFFER_END
         ;; three corners: (x,y) as two int16, then (u,v)
         (loop for (x y u v) in (list (list (+ dx sw) (+ dy sh) #x3F800000 #x3F800000)
                                      (list dx (+ dy sh) 0 #x3F800000)
                                      (list dx dy 0 0))
               for o from vstart by 12
               do (%bw o (logior (ash y 16) x)) (%bw (+ o 4) u) (%bw (+ o 8) v)))))
   ;; submit: invalidate, the batch, breadcrumb
   (let ((r *t4g-rcs*))
     (t4g-render-invalidate r)
     (t4g-emit r (list (ash #x31 23) *t4g-batch*))   ; MI_BATCH_BUFFER_START: global GTT (no PPGTT here)
     (let ((seq (t4g-render-breadcrumb r)))
       (t4g-submit r)
       seq)))
 

;;; ---- floats for the GPU -----------------------------------------------------------------------------
(defvar *t4g-f32-cell* (make-array 1 :element-type 'single-float :initial-element 0.0)
  "One single-float lane, for T4G-F32 to read a float's bits back out of.")

(defun t4g-f32 (x)
  "IEEE single bits of X (any real): what a vertex or a state word holds.  Stored into a
   single-float vector and its lane read back (on i386 a vector's data is at its word - 5): ~14 us
   a float through INTEGER-DECODE-FLOAT was 20 ms of a demo frame."
  (let ((v *t4g-f32-cell*))
    (setf (aref v 0) (coerce x 'single-float))
    (mem-ref (- (modus-int::%val->word v) 5) :u32)))

;;; ---- the EU: our own pixel shaders ----------------------------------------------------------------
;;; A gen 6 EU instruction is four dwords.  The two the shaders below use, with the fields that vary
;;; as parameters and the rest exactly as intel-gpu-tools' verified render-copy kernel has them:
;;;   PLN (8 lanes)  dst MRF DST <- the plane equation at GRF REG.SUB (floats a b - c, from setup)
;;;                  evaluated at the barycentrics in GRF BARY: one half of a SIMD16 channel.
;;;                  DW1 holds the destination register in bits 28:21, DW2 the plane's register in
;;;                  12:5 and its byte offset in 4:0, DW3 the barycentric register likewise.
;;;   SEND           render-target write, SIMD16, 8 message registers from m2 (R G B A, two each),
;;;                  binding table entry 0, end of thread.
(defun t4g-eu-pln (dst reg sub bary)
  (list #x0060005a (logior #x20000000 (ash dst 21) #x77be) (logior (ash reg 5) sub)
        (logior #x008d0000 (ash bary 5))))

(defparameter +t4g-eu-rt-write+ '(#x05800031 #x24001cc8 #x00000040 #x90019000))
(defparameter +t4g-eu-nop+ '(#x0000007e 0 0 0))

(defun t4g-ps-gouraud ()
  "A SIMD16 pixel shader: the one vertex attribute (a colour) interpolated across the triangle and
   written to render target 0.  The setup data starts at g6 (3DSTATE_WM's dispatch GRF 6): a
   channel's plane is 4 floats, so R at g6.0, G at g6.4, B at g7.0, A at g7.4; the barycentrics
   of the two halves at g2 and g4."
  (append
   (loop for (reg sub) in '((6 0) (6 16) (7 0) (7 16))
         for m from 2 by 2
         append (append (t4g-eu-pln m reg sub 2) (t4g-eu-pln (+ m 1) reg sub 4)))
   +t4g-eu-rt-write+
   (loop repeat 8 append +t4g-eu-nop+)))          ; the EU prefetches past the end

;;; ---- surfaces ----------------------------------------------------------------------------------------
(defstruct (t4g-surface (:conc-name t4s-)) gpu cpu w h pitch)

(defun t4g-surface-new (w h &key (align #x40000))
  "A W x H 32-bit surface in GPU memory (not mapped for the CPU), rows padded to 128 bytes (a Y tile's width: the depth
   buffer is Y-tiled), the height to 32 rows; 256 KB aligned (a display surface may be scanned out)."
  (let* ((pitch (* 128 (ceiling (* 4 w) 128))) (rows (* 32 (ceiling h 32))))
    (multiple-value-bind (gpu cpu) (t4g-alloc (* pitch rows) :align align :map nil)
      (make-t4g-surface :gpu gpu :cpu cpu :w w :h h :pitch pitch))))

(defun t4g-blt-clear (s value)
  "Fill surface S with VALUE on the blitter: every byte of every padded row (a constant fill is
   the same in any tiling, so it clears the Y-tiled depth buffer too).  The breadcrumb."
  (t4g-blt-fill (t4s-gpu s) (t4s-pitch s) 0 0 (floor (t4s-pitch s) 4) (* 32 (ceiling (t4s-h s) 32)) value)
  (let ((seq (t4g-breadcrumb *t4g-blt*))) (t4g-submit *t4g-blt*) seq))

;;; ---- triangles ----------------------------------------------------------------------------------------
;;; A vertex is 8 floats: position (x y z w) in SCREEN pixels with z in [0,1] (the clipper and the
;;; viewport transform are off: the CPU projects), then a colour (r g b a) in [0,1].  The VUE the
;;; pass-through VS writes is header, position, colour; SF reads the colour (offset 1, length 1).

(defparameter +t4g-tri-batch+ 65536)
(defvar *t4g-tri* (vector nil nil) "Triangle batches, (gpu . cpu): one per draw queued at a time.")

(defun t4g-draw-triangles (rt depth verts n &key (cull 1) texture (batch 0))
  "Draw N triangles (VERTS: a list of 8-float vertices, 3N of them) into surface RT, depth-tested
   against surface DEPTH (or none, NIL).  A vertex is a position and one attribute: a colour
   (Gouraud), or with TEXTURE (a surface) the coordinates (u v - -) it is sampled at, bilinearly
   (igt's sampling kernel).  CULL is SF's cull mode (1 none, 3 back).  BATCH picks the batch
   buffer, so two draws can be queued at once.  The breadcrumb."
  (unless (aref *t4g-tri* batch)
    (multiple-value-bind (g c) (t4g-alloc +t4g-tri-batch+) (setf (aref *t4g-tri* batch) (cons g c))))
  (let* ((b (car (aref *t4g-tri* batch))) (cpu (cdr (aref *t4g-tri* batch))) (p 0)
         (bt 1088) (ss 1120) (ss2 1152) (kern 1216) (vp 1600) (blend 1664) (ds 1728) (cc 1792)
         (sampler 1856) (vbase 4096)
         (ps (if texture (append *t4g-ps-copy* (loop repeat 8 append +t4g-eu-nop+)) (t4g-ps-gouraud))))
    (flet ((w (off v) (t4g-w (+ cpu off) v)))
      ;; state
      (w bt ss)
      (w ss (logior (ash 1 29) (ash #x0C0 18) (ash 1 13)))
      (w (+ ss 4) (t4s-gpu rt))
      (w (+ ss 8) (logior (ash (- (t4s-w rt) 1) 6) (ash (- (t4s-h rt) 1) 19)))
      (w (+ ss 12) (ash (- (t4s-pitch rt) 1) 3)) (w (+ ss 16) 0) (w (+ ss 20) 0)
      (when texture                                                  ; B8G8R8X8: alpha reads as 1
        (w (+ bt 4) ss2)
        (w ss2 (logior (ash 1 29) (ash #x0E9 18)))
        (w (+ ss2 4) (t4s-gpu texture))
        (w (+ ss2 8) (logior (ash (- (t4s-w texture) 1) 6) (ash (- (t4s-h texture) 1) 19)))
        (w (+ ss2 12) (ash (- (t4s-pitch texture) 1) 3)) (w (+ ss2 16) 0) (w (+ ss2 20) 0)
        ;; bilinear, clamped (TEXCOORDMODE_CLAMP 2), GL's LOD preclamp
        (w sampler (logior (ash 1 28) (ash 1 14) (ash 1 17)))
        (w (+ sampler 4) (logior 2 (ash 2 3) (ash 2 6))) (w (+ sampler 8) 0) (w (+ sampler 12) 0))
      (loop for d in ps for o from kern by 4 do (w o d))
      (w vp #xF99A130C) (w (+ vp 4) #x799A130C)
      (w blend 0) (w (+ blend 4) 3)                                  ; no blending; clamp
      (w ds 0) (w (+ ds 4) 0)
      (w (+ ds 8) (if depth (logior (ash 1 31) (ash 2 27) (ash 1 26)) 0))   ; LESS, test + write
      (dotimes (i 6) (w (+ cc (* 4 i)) 0))
      ;; vertices
      (let ((o vbase))
        (dolist (v verts)
          (dolist (f v) (w o (t4g-f32 f)) (incf o 4))))
      ;; commands
      (flet ((out (&rest ds) (dolist (d ds) (w p d) (incf p 4))))
        (out #x69040000)                                             ; PIPELINE_SELECT 3D
        (out (logior #x790D0000 1) 0 0)                              ; MULTISAMPLE: 1 sample
        (out #x78180000 1)                                           ; SAMPLE_MASK
        (out (logior #x61010000 8) 0 (logior b 1) (logior b 1) 0 (logior b 1) 0 1 0 1)
        (out #x61020000 0)                                           ; SIP
        (out (logior #x78050000 1) 24 0)                             ; URB
        (out (logior #x780D0000 (ash 1 12) 2) 0 0 vp)
        (out (logior #x78150000 3) 0 0 0 0) (out (logior #x78100000 4) 0 0 0 0 0)
        (out (logior #x78160000 3) 0 0 0 0) (out (logior #x78110000 5) 0 0 0 0 0 0)
        (out (logior #x78120000 2) 0 0 0)                            ; CLIP: pass-through
        (out (logior #x78170000 3) 0 0 0 0)
        ;; the depth buffer, after the flushes gen 6 wants before it (Mesa: depth stall, depth
        ;; cache flush, depth stall, each after the post-sync-nonzero pair)
        (out +pc4+ (logior (ash 1 20) (ash 1 1)) 0 0)
        (out +pc4+ (ash 1 14) (logior *t4g-scratch* 4) 0)
        (out +pc4+ (ash 1 13) 0 0) (out +pc4+ 1 0 0) (out +pc4+ (ash 1 13) 0 0)
        (if depth
            (out (logior #x79050000 5)
                 (logior (- (t4s-pitch depth) 1) (ash 1 18) (ash 1 26) (ash 1 27) (ash 1 29))
                 (t4s-gpu depth)
                 (logior (ash (- (t4s-w depth) 1) 6) (ash (- (t4s-h depth) 1) 19)) 0 0 0)
            (out (logior #x79050000 5) (logior (ash 7 29) (ash 1 18)) 0 0 0 0 0))
        (out #x79100000 0)                                           ; CLEAR_PARAMS
        (out (logior #x79000000 2) 0 (logior (ash (- (t4s-h rt) 1) 16) (- (t4s-w rt) 1)) 0)
        (out (logior #x780E0000 2) (logior blend 1) (logior ds 1) (logior cc 1))
        (out (logior #x78020000 (ash 1 12) 2) 0 0 (if texture sampler 0))
        (out (logior #x78130000 18) (logior (ash 1 22) (ash 1 11) (ash 1 4)) 0 (ash cull 29) (ash 2 25)
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)                          ; SF
        (out (logior #x78140000 7) kern (if texture (logior (ash 1 27) (ash 2 18)) (ash 1 18)) 0 (ash 6 16)
             (logior (ash 39 25) (ash 1 19) (ash 1 1)) (logior (ash 1 20) (ash 1 10)) 0 0)   ; WM
        (out (logior #x78090000 5)                                   ; header, position, colour
             (ash 1 25) (logior (ash 2 28) (ash 2 24) (ash 2 20) (ash 2 16))
             (ash 1 25) (logior (ash 1 28) (ash 1 24) (ash 1 20) (ash 1 16))
             (logior (ash 1 25) 16) (logior (ash 1 28) (ash 1 24) (ash 1 20) (ash 1 16)))
        (out (logior #x78010000 (ash 1 12) 2) 0 0 bt)
        (out (logior #x78080000 3) 32 (+ b vbase) (+ b +t4g-tri-batch+ -1) 0)   ; VB: pitch 32
        (out (logior #x7B000000 (ash #x04 10) 4) (* 3 n) 0 1 0 0)   ; 3DPRIMITIVE TRILIST
        (out (ash #x0A 23) 0)))                                      ; BATCH_BUFFER_END
    (let ((r *t4g-rcs*))
      (t4g-render-invalidate r)
      (t4g-emit r (list (ash #x31 23) b))
      (let ((seq (t4g-render-breadcrumb r))) (t4g-submit r) seq))))

;;; ---- the demo: spinning cubes, all on the GPU --------------------------------------------------------
;;; Five RGB cubes -- one in the middle, four orbiting on a tilted ring so they pass in front of it,
;;; behind it, and through each other -- drawn each frame by the render engine into a back buffer,
;;; depth-tested, Gouraud-shaded by our own pixel shader; the blitter clears colour and depth; the
;;; display flips to the finished buffer at vertical blank (DSPASURF latches there; DSPASURFLIVE
;;; says when it has).  The CPU only turns and projects 40 corners a frame.  The desk's screen,
;;; surface 0, is not touched: leaving flips back to it.

(defvar *t4g-demo* nil "The demo's surfaces: (front back depth).")
(defvar *t4d-desk* nil "The desk's own screen as a texture: surface 0.")
(defparameter *t4d-faces* '((0 2 6 4) (1 5 7 3) (0 4 5 1) (2 3 7 6) (0 1 3 2) (4 6 7 5)))

(defun %t4d-corners (cx cy cz size ax ay az)
  "A cube's 8 corners, centre (CX CY CZ), half-size SIZE, turned by AX AY AZ (radians) and
   projected to the screen (focal 900, near 1, far 60): a vector of (x y z w).  Corner I is
   (x y z) = the bits of I."
  (let* ((sx (sin ax)) (cxa (cos ax)) (sy (sin ay)) (cya (cos ay)) (sz (sin az)) (cza (cos az))
         (pts (make-array 8)))
    (dotimes (i 8 pts)
      (let* ((x (if (logbitp 0 i) size (- size))) (y (if (logbitp 1 i) size (- size)))
             (z (if (logbitp 2 i) size (- size))))
        ;; around x, then y, then z
        (psetq y (- (* y cxa) (* z sx)) z (+ (* y sx) (* z cxa)))
        (psetq x (+ (* x cya) (* z sy)) z (- (* z cya) (* x sy)))
        (psetq x (- (* x cza) (* y sz)) y (+ (* x sz) (* y cza)))
        (let ((wx (+ x cx)) (wy (+ y cy)) (wz (+ z cz)))
          (setf (aref pts i)
                (list (+ 683 (/ (* 900 wx) wz)) (- 384 (/ (* 900 wy) wz)) (/ (- wz 1) 59) 1.0)))))))

(defun %t4d-cube (tris cx cy cz size ax ay az)
  "TRIS with a cube's 12 triangles pushed on, each corner coloured by its bits: the RGB cube."
  (let ((pts (%t4d-corners cx cy cz size ax ay az)))
    (flet ((v (i) (append (aref pts i)
                          (list (if (logbitp 0 i) 1.0 0.15) (if (logbitp 1 i) 1.0 0.15)
                                (if (logbitp 2 i) 1.0 0.15) 1.0))))
      (dolist (f *t4d-faces* tris)
        (destructuring-bind (a b c d) f
          (push (v a) tris) (push (v b) tris) (push (v c) tris)
          (push (v a) tris) (push (v c) tris) (push (v d) tris))))))

(defun %t4d-cube-textured (cx cy cz size ax ay az)
  "A cube's 12 triangles, each face carrying texture coordinates over the whole texture."
  (let ((pts (%t4d-corners cx cy cz size ax ay az)) (tris nil))
    (flet ((v (i u vv) (append (aref pts i) (list u vv 0.0 0.0))))
      (dolist (f *t4d-faces* tris)
        (destructuring-bind (a b c d) f
          (push (v a 0.0 1.0) tris) (push (v b 1.0 1.0) tris) (push (v c 1.0 0.0) tris)
          (push (v a 0.0 1.0) tris) (push (v c 1.0 0.0) tris) (push (v d 0.0 0.0) tris))))))

(defun %t4d-scene (s)
  "The Gouraud vertices of the scene at S seconds (the four orbiting cubes)."
  (let ((tris nil))
    (dotimes (k 4 tris)
      (let ((a (+ (* s 0.8) (* k (/ pi 2)))))
        (setq tris (%t4d-cube tris (* 3.4 (cos a)) (* 1.2 (sin a)) (+ 9 (* 3.4 (sin a))) 0.7
                              (* s (+ 1.3 (* 0.2 k))) (* s 0.9) (* s (- 0.5 (* 0.1 k)))))))))

(defun %t4d-flip (s)
  "Scan out surface S from the next vertical blank; wait for it (at most 50 ms)."
  (t4-gpu-set #x7019C (t4s-gpu s))
  (let ((end (+ (get-internal-real-time) (floor internal-time-units-per-second 20))))
    (loop until (or (= (t4-gpu-reg #x701AC) (t4s-gpu s)) (>= (get-internal-real-time) end)))))

(defun %t4d-input-p ()
  "A key or a button pressed since the last call?  Drains the input queue."
  (%t4-poll)
  (let ((hit nil))
    (dolist (e *t4-events*)
      (let ((type (logand (floor e 1099511627776) #xFF)))
        (when (or (= type 4) (and (= type 12) (plusp (logand (ash e -48) 7)))) (setq hit t))))
    (setf *t4-events* nil)
    hit))

(defun t4g-demo (&key seconds)
  "Run the demo until a key or a click (or for SECONDS).  (frames seconds), and the screen is the
   desk's again."
  (unless *t4g-rcs* (t4g-render-start))
  (unless *t4g-demo*
    (setq *t4g-demo* (list (t4g-surface-new 1366 768) (t4g-surface-new 1366 768) (t4g-surface-new 1366 768))))
  (setq *t4d-desk* (make-t4g-surface :gpu 0 :cpu nil :w *t4-w* :h *t4-h* :pitch *t4-pitch*))
  (destructuring-bind (a b depth) *t4g-demo*
    (let* ((t0 (get-internal-real-time)) (hz internal-time-units-per-second)
           (frames 0) (back a) (bg #x0B1020) (one (t4g-f32 1.0)))
      (%t4d-input-p)
      (when *t4g-cursor* (t4-gpu-set #x70080 0) (t4-gpu-set #x70084 *t4g-cursor*))   ; no pointer
      (unwind-protect
           (loop
             (let ((s (/ (- (get-internal-real-time) t0) hz)))
               (when (or (and seconds (>= s seconds)) (and (not seconds) (%t4d-input-p)))
                 (return (list frames (float s))))
               (t4g-blt-clear back bg)
               (t4g-wait *t4g-blt* (t4g-blt-clear depth one) 100)
               ;; the orbiting cubes, Gouraud; the middle one wears the desk (surface 0) as a texture
               (let ((verts (%t4d-scene s))
                     (box (%t4d-cube-textured 0 0 9 1.5 (* s 0.7) (* s 1.1) (* s 0.3))))
                 (t4g-draw-triangles back depth verts (floor (length verts) 3))
                 (t4g-wait *t4g-rcs* (t4g-draw-triangles back depth box 12 :texture *t4d-desk* :batch 1) 100))
               (%t4d-flip back)
               (setq back (if (eq back a) b a))
               (incf frames)))
        ;; back to the desk: its surface, its pointer
        (t4-gpu-set #x7019C 0)
        (when *t4g-cursor* (t4g-cursor-on *t4-px* *t4-py*))))))

(defun %t4d-on-desk (desk dw dh)
  "app.lisp's desk-setup hook: the demo on the root menu."
  (declare (ignore dw dh))
  (%kiln-desk-register-action desk "GPU demo"
                              (lambda () (let ((r (t4g-demo))) (t4-log "gpu demo: ~D frames in ~,1F s" (first r) (second r))))))

(pushnew '%t4d-on-desk *kiln-desk-setup-hooks*)
