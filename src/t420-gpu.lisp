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

(defun t4g-alloc (bytes &key (align 4096))
  "BYTES of GPU memory from the pool: (values gpu-address cpu-address).  The CPU address is an
   uncached mapping of the aperture (one 32-bit access at a time: T4G-W / T4G-R)."
  (unless *t4g-next* (setq *t4g-next* *t4g-pool*))
  (let* ((gpu (* align (ceiling *t4g-next* align)))
         (cpu (modus-int::pc-mmio-map (+ (%t4g-aperture) gpu) bytes)))
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
