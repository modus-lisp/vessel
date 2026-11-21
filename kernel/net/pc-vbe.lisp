;;;; pc-vbe.lisp -- a VESA (VBE) linear framebuffer for the bare i386 PC image, chosen in Lisp.
;;;;
;;;; The PC's counterpart of the Pi's firmware framebuffer.  A BIOS answers only in real mode, so
;;;; the boot shim (modus scripts/pcboot2.s) leaves one routine behind, PM_BIOS, its address at
;;;; the boot record's +16 (0:0590): it drops to real mode, runs the interrupt the register block
;;;; at 0:05C0 describes, and comes back.  Everything else is here: the mode list, the panel's
;;;; native size from its EDID, the choice, the mode set, and the mapping.
;;;;
;;;;   block  +0 interrupt  +4 EAX  +8 EBX  +12 ECX  +16 EDX  +20 ESI  +24 EDI  +28 EBP
;;;;          +32 DS  +36 ES                                     (+40 FLAGS out)
;;;;
;;;; The mapping: boot/boot-i386-cl.lisp gives the image a 16 MB window at VA #x0C000000 whose four
;;;; page tables sit at PA = VA #x20000, empty.  Filling them is two stores a page: x86 caches no
;;;; not-present entry, so there is nothing to flush.  The window's VA is a fixnum on a 30-bit
;;;; tower; the framebuffer's own physical address (#xE0000000 and up) is not.
;;;;
;;;; Real-mode buffers: 0:24000 (controller info, 512 bytes), 0:24200 (mode info, 256), 0:24400
;;;; (EDID, 128) -- past the window's page tables, in what MODUS.COM used to occupy.  Not 3000:0,
;;;; PM_READ's: a code page fault between two calls here would overwrite it.
;;;;
;;;; State, for whoever draws (kiln's 1001/1005, the console): #x1B060 base VA, #x1B064 pitch in
;;;; bytes, #x1B068 width, #x1B06C height, #x1B070 bpp, #x1B074 the mode, #x1B078 1 when on.
;;;;
;;;; THE CONSOLE on it (%FB-CONSOLE-CHAR, pc-console.lisp's seam): the video BIOS's own 8x16
;;;; font (INT 10h AX=1130h, BH=6), copied to 0:24800 while the BIOS can still be asked; a
;;;; character grid in RAM at 0:25800 (#x1B080 columns, #x1B084 rows, #x1B088/#x1B08C the
;;;; cursor); the screen redrawn from the grid, never read -- a scroll jumps a third of the
;;;; screen, so a full redraw is rare.

(defun %pcbios-present-p ()
  (= (mem-ref #x580 :u32) #x54424350))          ; "PCBT", the shim's boot record

(defun %pcbios-int (n eax ebx ecx edx esi edi es)
  "Run BIOS interrupt N with these registers (ES a segment; DS = 0).  The registers come back
   in the block: read them with %PCBIOS-OUT.  NIL when there is no shim to ask."
  (when (%pcbios-present-p)
    (setf (mem-ref #x5C0 :u32) n
          (mem-ref #x5C4 :u32) eax (mem-ref #x5C8 :u32) ebx (mem-ref #x5CC :u32) ecx
          (mem-ref #x5D0 :u32) edx (mem-ref #x5D4 :u32) esi (mem-ref #x5D8 :u32) edi
          (mem-ref #x5DC :u32) 0 (mem-ref #x5E0 :u32) 0 (mem-ref #x5E4 :u32) es)
    ;; i386's indirect call strips a function's +3 tag; PM_BIOS keeps every register
    (call-native (+ (mem-ref #x590 :u32) 3))
    t))

(defun %pcbios-out (off) (mem-ref (+ #x5C0 off) :u32))
(defun %pcbios-ax () (logand (%pcbios-out 4) #xFFFF))

(defun %u16-at (a) (logior (mem-ref a :u8) (ash (mem-ref (+ a 1) :u8) 8)))

(defun %vbe-mode-list ()
  "The controller's modes (a list of numbers), its VBE version, or NIL."
  (let ((ci #x24000))
    (setf (mem-ref ci :u32) #x32454256)                         ; "VBE2": the 2.0+ fields
    (when (and (%pcbios-int #x10 #x4F00 0 0 0 0 0 (ash ci -4))
               (= (%pcbios-ax) #x004F)
               (= (mem-ref ci :u32) #x41534556))                ; "VESA"
      (let ((p (+ (* (%u16-at (+ ci 16)) 16) (%u16-at (+ ci 14))))
            (modes '()) (n 0))
        (loop (let ((m (%u16-at p)))
                (when (or (= m #xFFFF) (> n 512)) (return nil))
                (push m modes))
              (setq p (+ p 2) n (+ n 1)))
        (values (nreverse modes) (%u16-at (+ ci 4)))))))

(defun %vbe-edid-size ()
  "The panel's native width and height from its EDID's first detailed timing, or NIL."
  (let ((e #x24400))
    (when (and (%pcbios-int #x10 #x4F15 1 0 0 0 (logand e #xF) (ash e -4))
               (= (%pcbios-ax) #x004F)
               (= (mem-ref e :u32) #xFFFFFF00))                 ; the EDID header
      (let ((w (logior (mem-ref (+ e 56) :u8) (ash (logand (mem-ref (+ e 58) :u8) #xF0) 4)))
            (h (logior (mem-ref (+ e 59) :u8) (ash (logand (mem-ref (+ e 61) :u8) #xF0) 4))))
        (when (and (> w 0) (> h 0)) (values w h))))))

(defun %vbe-mode-info (mode version)
  "(width height pitch bpp physbase) of MODE if it is a 32-bit direct-colour linear mode that
   fits the 16 MB window, else NIL."
  (let ((mi #x24200))
    (when (and (%pcbios-int #x10 #x4F01 0 mode 0 0 (logand mi #xF) (ash mi -4))
               (= (%pcbios-ax) #x004F))
      (let ((attr (%u16-at mi))
            (w (%u16-at (+ mi #x12))) (h (%u16-at (+ mi #x14)))
            (bpp (mem-ref (+ mi #x19) :u8)) (model (mem-ref (+ mi #x1B) :u8))
            (pitch (if (>= version #x300) (%u16-at (+ mi #x32)) (%u16-at (+ mi #x10))))
            (base (mem-ref (+ mi #x28) :u32)))
        (when (and (= (logand attr #x91) #x91)                  ; supported, graphics, linear
                   (= bpp 32) (= model 6) (> pitch 0) (not (zerop base))
                   (<= (* pitch h) #x1000000))
          (list w h pitch bpp base))))))

(defun %vbe-choose (&optional max-w max-h)
  "The mode to set: the panel's native size if a mode has it, else the largest that fits it
   (or MAX-W x MAX-H, or 1920 x 1200 with no EDID).  Returns (mode . info) or NIL."
  (multiple-value-bind (modes version) (%vbe-mode-list)
    (when modes
      (multiple-value-bind (nw nh) (%vbe-edid-size)
        (let ((cw (or max-w nw 1920)) (ch (or max-h nh 1200)) (best nil) (area 0))
          (dolist (m modes)
            (let ((i (%vbe-mode-info m version)))
              (when i
                (let ((w (first i)) (h (second i)))
                  (cond ((and nw (= w nw) (= h nh) (null max-w))
                         (return-from %vbe-choose (cons m i)))
                        ((and (<= w cw) (<= h ch) (> (* w h) area))
                         (setq best (cons m i) area (* w h))))))))
          best)))))

(defun %vbe-map (base bytes)
  "Point the framebuffer window's pages at physical BASE, BYTES of it."
  (let ((n (ceiling bytes 4096)) (i 0))
    (loop (when (>= i n) (return nil))
      (setf (mem-ref (+ #x20000 (* i 4)) :u32) (logior (+ base (* i 4096)) 3))
      (setq i (+ i 1)))))

(defun vbe-on-p () (= (mem-ref #x1B078 :u32) 1))
(defun vbe-base () (mem-ref #x1B060 :u32))
(defun vbe-pitch () (mem-ref #x1B064 :u32))
(defun vbe-width () (mem-ref #x1B068 :u32))
(defun vbe-height () (mem-ref #x1B06C :u32))

(defun vbe-init (&optional max-w max-h)
  "Set the best linear 32-bit mode and map it at #x0C000000.  Returns (width height) or NIL
   (no shim, no VBE 2, or no such mode: the screen stays in text mode)."
  (let ((c (and (%pcbios-present-p) (%vbe-choose max-w max-h))))
    (when c
      (destructuring-bind (mode w h pitch bpp base) c
        (when (and (%pcbios-int #x10 #x4F02 (logior mode #x4000) 0 0 0 0 0)
                   (= (%pcbios-ax) #x004F))
          (%vbe-map base (* pitch h))
          (setf (mem-ref #x1B060 :u32) #x0C000000 (mem-ref #x1B064 :u32) pitch
                (mem-ref #x1B068 :u32) w (mem-ref #x1B06C :u32) h
                (mem-ref #x1B070 :u32) bpp (mem-ref #x1B074 :u32) mode)
          (%fbc-init)
          (setf (mem-ref #x1B078 :u32) 1)          ; the console draws here from now on
          (list w h))))))

(defun vbe-fill (x y w h rgb)
  "Fill a rectangle with 0x00RRGGBB.  Writes only: the framebuffer is uncached, never read it."
  (let* ((pitch (vbe-pitch)) (row (+ (vbe-base) (* y pitch) (* x 4))) (j 0))
    (loop (when (>= j h) (return nil))
      (let ((p row) (end (+ row (* w 4))))
        (loop (when (>= p end) (return nil))
          (setf (mem-ref p :u32) rgb)
          (setq p (+ p 4))))
      (setq row (+ row pitch) j (+ j 1)))))

(defun vbe-test-pattern ()
  "Eight vertical colour bars over the whole screen, a white frame round them."
  (when (vbe-on-p)
    (let* ((w (vbe-width)) (h (vbe-height)) (bw (floor w 8))
           (colours '(#xFFFFFF #xFFFF00 #x00FFFF #x00FF00 #xFF00FF #xFF0000 #x0000FF #x000000))
           (i 0))
      (dolist (c colours)
        (vbe-fill (* i bw) 0 (if (= i 7) (- w (* 7 bw)) bw) h c)
        (setq i (+ i 1)))
      (vbe-fill 0 0 w 4 #xFFFFFF) (vbe-fill 0 (- h 4) w 4 #xFFFFFF)
      (vbe-fill 0 0 4 h #xFFFFFF) (vbe-fill (- w 4) 0 4 h #xFFFFFF)
      (list w h))))

;;; ---------------------------------------------------------------------------
;;; The console

(defun %fbc-cols () (mem-ref #x1B080 :u32))
(defun %fbc-rows () (mem-ref #x1B084 :u32))
(defun %fbc-x () (mem-ref #x1B088 :u32))
(defun %fbc-y () (mem-ref #x1B08C :u32))
(defun %fbc-set-xy (x y) (setf (mem-ref #x1B088 :u32) x (mem-ref #x1B08C :u32) y))

(defun %fbc-init ()
  "The BIOS font, an empty grid, a black screen."
  (when (%pcbios-int #x10 #x1130 #x0600 0 0 0 0 0)
    (let ((src (+ (* (logand (%pcbios-out 36) #xFFFF) 16) (logand (%pcbios-out 28) #xFFFF)))
          (i 0))
      (loop (when (>= i 4096) (return nil))
        (setf (mem-ref (+ #x24800 i) :u8) (mem-ref (+ src i) :u8))
        (setq i (+ i 1)))))
  (let ((cols (min 240 (floor (vbe-width) 8))) (rows (min 75 (floor (vbe-height) 16))) (i 0))
    (setf (mem-ref #x1B080 :u32) cols (mem-ref #x1B084 :u32) rows)
    (loop (when (>= i (* cols rows)) (return nil))
      (setf (mem-ref (+ #x25800 i) :u8) 32)
      (setq i (+ i 1)))
    (%fbc-set-xy 0 0)
    (vbe-fill 0 0 (vbe-width) (vbe-height) 0)
    (%fbc-cursor t)))

(defun %fbc-glyph (col row code)
  "Cell (COL, ROW) drawn as CODE, light grey on black, 8 x 16."
  (let* ((pitch (vbe-pitch)) (font (+ #x24800 (* (logand code #xFF) 16)))
         (line (+ (vbe-base) (* row 16 pitch) (* col 32))) (r 0))
    (loop (when (>= r 16) (return nil))
      (let ((bits (mem-ref (+ font r) :u8)) (p line) (m 128))
        (loop (when (zerop m) (return nil))
          (setf (mem-ref p :u32) (if (zerop (logand bits m)) 0 #xC0C0C0))
          (setq p (+ p 4) m (ash m -1))))
      (setq line (+ line pitch) r (+ r 1)))))

(defun %fbc-cursor (on)
  "The cursor: a bar under the cell at the cursor (ON), or the cell as it is."
  (let ((x (%fbc-x)) (y (%fbc-y)))
    (when (and (< x (%fbc-cols)) (< y (%fbc-rows)))
      (%fbc-glyph x y (mem-ref (+ #x25800 (* y (%fbc-cols)) x) :u8))
      (when on (vbe-fill (* x 8) (+ (* y 16) 14) 8 2 #xC0C0C0)))))

(defun %fbc-redraw ()
  (let ((cols (%fbc-cols)) (rows (%fbc-rows)) (y 0))
    (loop (when (>= y rows) (return nil))
      (let ((x 0))
        (loop (when (>= x cols) (return nil))
          (%fbc-glyph x y (mem-ref (+ #x25800 (* y cols) x) :u8))
          (setq x (+ x 1))))
      (setq y (+ y 1)))))

(defun %fbc-scroll (n)
  "The grid up N rows, the screen redrawn from it."
  (let* ((cols (%fbc-cols)) (rows (%fbc-rows)) (keep (* (- rows n) cols)) (i 0))
    (loop (when (>= i keep) (return nil))
      (setf (mem-ref (+ #x25800 i) :u8) (mem-ref (+ #x25800 i (* n cols)) :u8))
      (setq i (+ i 1)))
    (loop (when (>= i (* rows cols)) (return nil))
      (setf (mem-ref (+ #x25800 i) :u8) 32)
      (setq i (+ i 1)))
    (%fbc-redraw)))

(defun %fbc-newline ()
  (let ((y (+ (%fbc-y) 1)) (rows (%fbc-rows)))
    (when (>= y rows)
      (let ((n (max 1 (floor rows 3))))
        (%fbc-scroll n)
        (setq y (- y n))))
    (%fbc-set-xy 0 y)))

(defun %fb-console-char (code)
  "pc-console's seam: CODE on the framebuffer console."
  (%fbc-cursor nil)
  (cond ((= code 10) (%fbc-newline))
        ((= code 13) (%fbc-set-xy 0 (%fbc-y)))
        ((= code 8) (when (> (%fbc-x) 0) (%fbc-set-xy (- (%fbc-x) 1) (%fbc-y))))
        ((>= code 32)
         (let ((x (%fbc-x)) (y (%fbc-y)))
           (setf (mem-ref (+ #x25800 (* y (%fbc-cols)) x) :u8) (if (< code 256) code 63))
           (%fbc-glyph x y code)
           (if (>= (+ x 1) (%fbc-cols)) (%fbc-newline) (%fbc-set-xy (+ x 1) y)))))
  (%fbc-cursor t)
  code)
