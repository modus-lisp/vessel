;;;; mac-console.lisp — the bare 68k Mac's screen console.
;;;;
;;;; Output: every WRITE-CHAR-SERIAL compiled into the image also reaches
;;;; %CONSOLE-WRITE-CHAR (the build sets the compiler's write-char-serial hook,
;;;; as the PC and Pi console builds do), which echoes to serial port A and draws
;;;; the character on the Mac's screen in the 8x8 font.  Input needs nothing
;;;; here: the translator's getc (translate-68k +68K-MAC-CONSOLE-CODE+) already
;;;; takes a key from the ROM's event queue or a byte from the serial port,
;;;; whichever comes first, so READ-CHAR-SERIAL is the keyboard too.
;;;;
;;;; The screen is the one the ROM set up and drew the Happy Mac on: its base is
;;;; ScrnBase ($824), its row stride ScreenRow ($106), and its depth and size are
;;;; the main graphics device's pixel map (MainDevice $8A4 -> GDevice ->
;;;; gdPMap at +22 -> PixMap: bounds at +6, pixelSize at +32).  Depths 1, 8,
;;;; 16 and 32: black text on white, as a Mac draws it.
;;;;
;;;; A TEXT-MODE BUFFER, as a terminal keeps one: the screen's characters, a
;;;; byte a cell, COLS x ROWS at #x1000A000, its rows a RING whose first is the
;;;; screen's top (#x10009224).  A character is stored there and its glyph
;;;; drawn from the bitmap font -- one byte a pixel row on a 1-bit screen.  At
;;;; the bottom the screen SCROLLS: its pixels move up a text row in one
;;;; _BlockMove (SYSCALL3 service 2: the ROM's), the bottom row is cleared, and
;;;; the ring turns.  No scrollback: a row that leaves the top is gone.
;;;; MACC-REDRAW paints the whole screen from the buffer.
;;;;
;;;; State, in the low block's argv area (a Mac image has no argv; the serial
;;;; parameter blocks are below #x10009200):
;;;;   #x10009200 cursor x   #x10009204 cursor y   #x10009208 cols
;;;;   #x1000920C rows       #x10009210 base       #x10009214 rowbytes
;;;;   #x10009218 depth      #x1000921C on (1 once %CONSOLE-INIT has run)
;;;;   #x10009220 height (pixels)   #x10009224 the buffer's top row
;;;;   #x1000A000 the text buffer, up to 8 KB (cols x rows is capped to it)
;;;;
;;;; THE TEXT IS INSET by +MACC-PAD-X+ / +MACC-PAD-Y+ pixels from the screen's
;;;; edge (a compact Mac's corners are rounded and its bezel covers the edge);
;;;; %CONSOLE-INIT first clears the WHOLE screen to white, over the ROM's
;;;; desktop pattern.
;;;;   #x10009400 the font, 95 glyphs x 8 bytes
;;;;   #x10009700 a staging line, 2 KB (a screen at a HIGH address: see MACC-HIGH-P)
;;;; The build defines MACC-FONT-BYTES before this text.

(defun macc-x ()        (mem-ref #x10009200 :u32))
(defun macc-y ()        (mem-ref #x10009204 :u32))
(defun macc-cols ()     (mem-ref #x10009208 :u32))
(defun macc-rows ()     (mem-ref #x1000920C :u32))
(defun macc-base ()
  ;; signed, as MACC-SCRNBASE made it: stored as a 32-bit word, a high screen's
  ;; base read back as :U32 is a positive bignum, and MACC-HIGH-P said no
  (let ((hi (mem-ref #x10009210 :u16)))
    (if (>= hi #xC000)
        (+ (* (- hi #x10000) #x10000) (mem-ref #x10009212 :u16))
        (mem-ref #x10009210 :u32))))
(defun macc-rowbytes () (mem-ref #x10009214 :u32))
(defun macc-depth ()    (mem-ref #x10009218 :u32))
(defun macc-set-x (v)   (setf (mem-ref #x10009200 :u32) v))
(defun macc-set-y (v)   (setf (mem-ref #x10009204 :u32) v))
(defconstant +macc-pad-x+ 8 "Left / right margin, pixels (a multiple of 8).")
(defconstant +macc-pad-y+ 6 "Top / bottom margin, pixels.")

(defun macc-deref (h)
  "The pointer a handle H holds, or 0.  Its low 24 bits: the boot ROM's Memory
   Manager may run 24-bit, its master pointers' top byte then holds the lock /
   purge / resource flags (a Quadra's), and those pointers are in the system
   heap, far below 16 MB.  Read as halves: a flagged pointer is no fixnum."
  (if (or (zerop h) (>= h #x20000000))
      0
      (+ (* (logand (mem-ref h :u16) #xFF) #x10000) (mem-ref (+ h 2) :u16))))

(defun macc-scrnbase ()
  "ScrnBase ($824).  A screen at #xC0000000 or above -- the SE/30's at
   #xFEE08040, the Quadras' and every NuBus card's in the slot space -- is
   kept as the NEGATIVE fixnum with the same low 32 bits: as a :U32 read it is
   a bignum, which no address arithmetic or MEM-REF can use (MACC-HIGH-P)."
  (let ((hi (mem-ref #x824 :u16)))
    (if (>= hi #xC000)
        (+ (* (- hi #x10000) #x10000) (mem-ref #x826 :u16))
        (mem-ref #x824 :u32))))

(defun macc-high-p ()
  "Is the screen at a high address?  MEM-REF cannot reach one: it untags its
   address with a LOGICAL shift (compile-mem-ref: what lets a 32-bit port reach
   #x40000000-#x7FFFFFFF), so a negative fixnum lands at the wrong place.  Such
   a screen is written by staging pixels at #x10009700 and copying them with
   SYSCALL3 service 4 (MACC-BLIT), whose untag is arithmetic."
  (< (macc-base) 0))

(defun macc-stage () (%conv-addr #x10009700))

(defun macc-blit (src dst n)
  "N bytes from SRC (low memory) to DST (the screen, anywhere): SYSCALL3
   service 4, a plain copy.  Not _BlockMove (service 2): on a 68040 it copies
   with MOVE16 line bursts, which the Quadras' video memory does not take --
   MAME's Quadra 950 went into its interrupt handlers until the stack ran
   into the heap."
  (syscall3 4 src dst n))

(defun macc-probe ()
  "Read the screen the ROM set up into the console's words."
  (let* ((base (macc-scrnbase))
         (rowbytes (logand (mem-ref #x106 :u16) #x3FFF))
         ;; MainDevice only with Color QuickDraw (ROM85 $28E bit 14 clear):
         ;; on a Classic the word is not a handle
         (gd (if (zerop (logand (mem-ref #x28E :u16) #x4000))
                 (macc-deref (mem-ref #x8A4 :u32))
                 0))
         (pm (if (zerop gd) 0 (macc-deref (mem-ref (+ gd 22) :u32))))
         (depth (if (zerop pm) 1 (mem-ref (+ pm 32) :u16)))
         (width (if (zerop pm)
                    (* rowbytes 8)
                    (- (mem-ref (+ pm 12) :u16) (mem-ref (+ pm 8) :u16))))
         (height (if (zerop pm)
                     342
                     (- (mem-ref (+ pm 10) :u16) (mem-ref (+ pm 6) :u16)))))
    (unless (member depth '(1 8 16 32)) (setq depth 1))
    (setf (mem-ref #x10009210 :u32) base)
    (setf (mem-ref #x10009214 :u32) rowbytes)
    (setf (mem-ref #x10009218 :u32) depth)
    (setf (mem-ref #x10009220 :u32) height)
    (let ((cols (floor (- width (* 2 +macc-pad-x+)) 8))
          (rows (floor (- height (* 2 +macc-pad-y+)) 8)))
      ;; the text buffer holds 8 KB
      (when (> (* cols rows) 8192) (setq rows (floor 8192 cols)))
      (setf (mem-ref #x10009208 :u32) cols)
      (setf (mem-ref #x1000920C :u32) rows))))

(defun macc-clear-screen ()
  "The whole screen white, in its depth: 0 at 1 and 8 bits, #x7FFF a pixel
   at 16, #x00FFFFFF at 32 (written as two 16-bit halves: no bignum)."
  (when (macc-high-p) (return-from macc-clear-screen (macc-clear-screen-high)))
  (let* ((a (macc-base)) (d (macc-depth))
         (end (+ a (* (macc-rowbytes) (mem-ref #x10009220 :u32)))))
    (loop
      (when (>= a end) (return nil))
      (cond ((= d 16) (setf (mem-ref a :u16) #x7FFF) (setq a (+ a 2)))
            ((= d 32) (setf (mem-ref a :u16) #x00FF) (setf (mem-ref (+ a 2) :u16) #xFFFF)
                      (setq a (+ a 4)))
            (t (setf (mem-ref a :u16) 0) (setq a (+ a 2)))))))

(defun macc-stage-white ()
  "The staging line (2 KB) white in this depth."
  (let ((d (macc-depth)) (st (macc-stage)) (i 0))
    (loop
      (when (>= i 2048) (return nil))
      (cond ((= d 16) (setf (mem-ref (+ st i) :u16) #x7FFF) (setq i (+ i 2)))
            ((= d 32) (setf (mem-ref (+ st i) :u16) #x00FF)
                      (setf (mem-ref (+ st i 2) :u16) #xFFFF) (setq i (+ i 4)))
            (t (setf (mem-ref (+ st i) :u16) 0) (setq i (+ i 2)))))))

(defun macc-white-lines (line width count)
  "COUNT scan lines of WIDTH bytes from screen address LINE white, from a
   staging line MACC-STAGE-WHITE made, in pieces of at most 2 KB."
  (let ((rb (macc-rowbytes)) (st (macc-stage)) (y 0))
    (loop
      (when (>= y count) (return nil))
      (let ((off 0))
        (loop
          (when (>= off width) (return nil))
          (macc-blit st (+ line off) (if (> (- width off) 2048) 2048 (- width off)))
          (setq off (+ off 2048))))
      (setq line (+ line rb))
      (setq y (+ y 1)))))

(defun macc-clear-screen-high ()
  "MACC-CLEAR-SCREEN for a high screen."
  (macc-stage-white)
  (macc-white-lines (macc-base) (macc-rowbytes) (mem-ref #x10009220 :u32)))

(defun macc-put-pixel-row (addr bits)
  "Eight pixels of a glyph row (BITS, MSB leftmost) at ADDR in this depth."
  (let ((d (macc-depth)) (x 0))
    (cond
      ((= d 1) (setf (mem-ref addr :u8) bits))
      (t
       (loop
         (when (>= x 8) (return nil))
         (let ((ink (not (zerop (logand bits (ash 128 (- x)))))))
           (cond
             ((= d 8)  (setf (mem-ref (+ addr x) :u8) (if ink #xFF 0)))
             ((= d 16) (setf (mem-ref (+ addr (* x 2)) :u16) (if ink 0 #x7FFF)))
             (t        (setf (mem-ref (+ addr (* x 4)) :u32) (if ink 0 #xFFFFFF)))))
         (setq x (+ x 1)))))))

(defun macc-cell-addr (col row)
  (+ (macc-base) (* (+ (* row 8) +macc-pad-y+) (macc-rowbytes))
     (floor (* (+ (* col 8) +macc-pad-x+) (macc-depth)) 8)))

(defun macc-render (code col row)
  ;; The screen's geometry read ONCE: on a Mac Classic a glyph was ~20 calls
  ;; (MACC-ROWBYTES a row, MACC-DEPTH a row inside MACC-PUT-PIXEL-ROW) and the
  ;; console a quarter of a REPL form's time (MAME's PC samples).  On a 1-bit
  ;; screen a glyph row is one byte.
  (let* ((g (if (and (>= code 32) (<= code 126)) (- code 32) 0))
         ;; an address kept as a VALUE: %CONV-ADDR, so it moves with the region
         (font (+ (%conv-addr #x10009400) (* g 8)))
         (rb (macc-rowbytes))
         (d (macc-depth))
         (addr (+ (macc-base) (* (+ (* row 8) +macc-pad-y+) rb)
                  (floor (* (+ (* col 8) +macc-pad-x+) d) 8)))
         (r 0))
    (cond
      ((< addr 0)
       ;; a high screen (MACC-HIGH-P): each row staged, then _BlockMove'd
       (let ((st (macc-stage)))
         (loop
           (when (>= r 8) (return nil))
           (if (= d 1)
               (setf (mem-ref st :u8) (mem-ref (+ font r) :u8))
               (macc-put-pixel-row st (mem-ref (+ font r) :u8)))
           (macc-blit st addr d)
           (setq addr (+ addr rb))
           (setq r (+ r 1)))))
      ((= d 1)
        (loop
          (when (>= r 8) (return nil))
          (setf (mem-ref addr :u8) (mem-ref (+ font r) :u8))
          (setq addr (+ addr rb))
          (setq r (+ r 1))))
      (t
        (loop
          (when (>= r 8) (return nil))
          (macc-put-pixel-row addr (mem-ref (+ font r) :u8))
          (setq addr (+ addr rb))
          (setq r (+ r 1)))))))

(defun macc-clear-row (row)
  ;; On a 1-bit screen a blank text row is 8 pixel lines of zero bytes (a
  ;; space's glyph is all zeros) -- it was a space rendered in every column,
  ;; over a thousand calls a newline.  Deeper screens still render spaces.
  (when (macc-high-p)
    ;; a high screen: the text row's 8 scan lines white, staged
    (let* ((d (macc-depth)) (rb (macc-rowbytes)))
      (macc-stage-white)
      (macc-white-lines (+ (macc-base) (* (+ (* row 8) +macc-pad-y+) rb) (floor (* +macc-pad-x+ d) 8))
                        (floor (* (macc-cols) 8 d) 8) 8)
      (return-from macc-clear-row nil)))
  (if (= (macc-depth) 1)
      (let* ((rb (macc-rowbytes)) (n (macc-cols))
             (line (+ (macc-base) (* (+ (* row 8) +macc-pad-y+) rb) (floor +macc-pad-x+ 8)))
             (r 0))
        (loop
          (when (>= r 8) (return nil))
          (let ((a line) (e (+ line n)))
            (loop
              (when (>= a e) (return nil))
              (setf (mem-ref a :u8) 0)
              (setq a (+ a 1))))
          (setq line (+ line rb))
          (setq r (+ r 1))))
      (let ((c 0) (n (macc-cols)))
        (loop
          (when (>= c n) (return nil))
          (macc-render 32 c row)
          (setq c (+ c 1))))))

;;; ---- the text buffer ----

(defun macc-cell (col row)
  "The buffer address of screen cell COL, ROW (the ring's top is row 0)."
  (let ((rows (macc-rows)))
    (+ (%conv-addr #x1000A000)
       (* (mod (+ (mem-ref #x10009224 :u32) row) rows) (macc-cols))
       col)))

(defun macc-blank-buffer-row (row)
  (let* ((a (macc-cell 0 row)) (e (+ a (macc-cols))))
    (loop
      (when (>= a e) (return nil))
      (setf (mem-ref a :u8) 32)
      (setq a (+ a 1)))))

(defun macc-put (code col row)
  "CODE at COL, ROW: in the buffer, and on the screen."
  (setf (mem-ref (macc-cell col row) :u8) code)
  (macc-render code col row))

(defun macc-redraw ()
  "Paint the whole screen from the text buffer."
  (let ((row 0) (rows (macc-rows)) (cols (macc-cols)))
    (loop
      (when (>= row rows) (return nil))
      (let ((col 0))
        (loop
          (when (>= col cols) (return nil))
          (macc-render (mem-ref (macc-cell col row) :u8) col row)
          (setq col (+ col 1))))
      (setq row (+ row 1)))))

(defun macc-scroll ()
  "Up one text row: the pixels in one _BlockMove, the bottom row cleared, the
   buffer's ring turned (its old top row becomes the blank bottom)."
  (let* ((rb (macc-rowbytes)) (rows (macc-rows))
         (top (+ (macc-base) (* +macc-pad-y+ rb))))
    ;; a high screen by plain copy (MACC-BLIT's reason); a low one, a 68000
    ;; Mac's, by the ROM's _BlockMove, which is faster there
    (syscall3 (if (< top 0) 4 2) (+ top (* 8 rb)) top (* (- rows 1) 8 rb))
    (setf (mem-ref #x10009224 :u32) (mod (+ (mem-ref #x10009224 :u32) 1) rows))
    (macc-blank-buffer-row (- rows 1))
    (macc-clear-row (- rows 1))))

(defun macc-newline ()
  (macc-set-x 0)
  (let ((ny (+ (macc-y) 1)))
    (if (>= ny (macc-rows))
        (macc-scroll)
        (macc-set-y ny))))

(defun macc-write-char (c)
  (cond
    ((= c 10) (macc-newline))
    ((= c 13) (macc-set-x 0))
    ((= c 8)
     (when (> (macc-x) 0)
       (macc-set-x (- (macc-x) 1))
       (macc-put 32 (macc-x) (macc-y))))
    ((>= c 32)
     (macc-put c (macc-x) (macc-y))
     (let ((nx (+ (macc-x) 1)))
       (if (>= nx (macc-cols)) (macc-newline) (macc-set-x nx))))))

;;; ------------------------------------------------------------
;;; The seams
;;; ------------------------------------------------------------

(defun %console-init ()
  ;; a Mac OS application (the static block's #x10006054, translate-68k
  ;; +68K-MAC-HOST+): the application draws the console in its window, and
  ;; the screen is not ours (and a saved core's "on" is put back off)
  (if (zerop (mem-ref #x10006054 :u32))
      (macc-take-screen)
      (setf (mem-ref #x1000921C :u32) 0)))

(defun macc-take-screen ()
  (let ((p (macc-font-bytes)) (i 0))
    (loop
      (when (null p) (return nil))
      (setf (mem-ref (+ #x10009400 i) :u8) (car p))
      (setq p (cdr p)) (setq i (+ i 1))))
  (macc-probe)
  (macc-clear-screen)
  (setf (mem-ref #x10009224 :u32) 0)
  (let ((r 0) (n (macc-rows)))
    (loop
      (when (>= r n) (return nil))
      (macc-blank-buffer-row r)
      (setq r (+ r 1))))
  (macc-set-x 0) (macc-set-y 0)
  (setf (mem-ref #x1000921C :u32) 1))

(defun %console-write-char (code)
  ;; The write-char-serial hook: serial port A always, then the screen, once
  ;; %CONSOLE-INIT has found it.
  (write-char-serial code)
  (when (= (mem-ref #x1000921C :u32) 1) (macc-write-char code)))

(defun %console-read-char ()
  ;; a key, or a byte on serial port A (the translator's getc polls both); a
  ;; Return arrives as 13, which the REPL's line reader wants as 10
  (let ((c (read-char-serial)))
    (if (= c 13) 10 c)))
