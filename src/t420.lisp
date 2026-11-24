;;;; t420.lisp — kiln's desk on a PC running bare-metal modus on 32-bit x86: the ThinkPad
;;;; T420 first.  The desk on the VBE framebuffer, the PS/2 keyboard as keys and the PS/2 aux
;;;; port (the TrackPoint and the touchpad, which the firmware presents as one PS/2 mouse) as
;;;; the pointer.
;;;;
;;;; The platform layer for app.lisp, as zero.lisp is for the Pi Zero, over the bare i386
;;;; image's own drivers: kernel/net/pc-vbe.lisp (VBE through the pcboot shim's PM_BIOS, the
;;;; framebuffer mapped at #x0E000000) and plain port I/O for the 8042.  `kiln t420' installs
;;;; this file, app.lisp, glass and scribe into the image under QEMU and saves the core; the
;;;; core's restart hook is KILN-T420-MAIN.
;;;;
;;;; The calls, by number, as app.lisp makes them:
;;;;   1001 size (0 width, 1 height, 2 scale = 1)   1002 fill   1003 present (nothing to do)
;;;;   1004 next event                               1005 blit (unused: see %KILN-BLIT-REGION)
;;;;   1006 keyboard (it is always there)            1010 speaker open (none: non-zero)
;;;; Events are app.lisp's encoding: type << 40 | y << 20 | x, type 1 press, 2 drag, 3 release,
;;;; 5 hover, 4 a key with its X11 keysym in the low bits.
;;;;
;;;; THE BLIT IS DONE HERE, not through 1005: app.lisp's %KILN-BLIT-REGION hands the platform an
;;;; address computed for a 64-bit heap (%GC-WORD-OF + 7, 8 bytes a pixel).  On i386 a word is 4
;;;; bytes and an object's payload is not at +7, so t420-desk.lisp (loaded after app.lisp)
;;;; replaces %KILN-BLIT-REGION with one that reads the pixel vector with AREF.

(in-package :cl-user)

;;; app.lisp names the media player's symbols; without the player the packages still have to
;;; exist for the file to read (as zero.lisp does).
(unless (find-package "WARP-MEDIA")
  (make-package "WARP-MEDIA" :use nil)
  (dolist (n '("FOLDER-TRACKS" "LIBRARY-PLAYER" "MAKE-LIBRARY" "PLAY-PATH"))
    (export (intern n "WARP-MEDIA") "WARP-MEDIA")))
(unless (find-package "WARP-MEDIA-GLASS")
  (make-package "WARP-MEDIA-GLASS" :use nil)
  (export (intern "MAKE-MEDIA-WINDOW" "WARP-MEDIA-GLASS") "WARP-MEDIA-GLASS"))

;;; --- the screen ------------------------------------------------------------------------------

(defvar *t4-w* 0)
(defvar *t4-h* 0)
(defvar *t4-base* 0)
(defvar *t4-pitch* 0)

(defun %t4-screen-up (&optional max-w max-h)
  "Set the VBE mode and take the screen from the text console.  NIL when there is no VBE
   (QEMU booted with -kernel has no pcboot shim, so no BIOS)."
  (let ((wh (modus-int::vbe-init max-w max-h)))
    (when wh
      (setf *t4-w* (first wh) *t4-h* (second wh)
            *t4-base* (modus-int::vbe-base) *t4-pitch* (modus-int::vbe-pitch))
      ;; the console draws on the framebuffer while #x1B078 is 1 (pc-console.lisp); 2 sends
      ;; it to VGA text memory, which a graphics mode does not show -- the desk owns the screen
      (setf (mem-ref #x1B078 :u32) 2)
      wh)))

;;; THE PANEL'S OWN RESOLUTION.  The T420's BIOS offers no 16:9 mode (its best: 1024x768 and
;;; 960x600; the panel is 1366x768), so the VBE mode is a 4:3 picture the CPU's panel fitter
;;; stretches -- the wrong shape.  Measured on the T420's Intel HD 3000: pipe A's TIMINGS are the
;;; panel's native 1366x768 already (HTOTAL/VTOTAL active), only its SOURCE size (PIPEASRC) is
;;; 1024x768 and the panel fitter (PF_CTL) scales it.  So: source = native, fitter off, plane A's
;;; stride for the wider rows, and the surface register rewritten to latch it.  Only when the GPU
;;; is Intel's, pipe A's active size is the EDID's, and the VBE mode is not already it.
(defvar *t4-native* t)     ; switch to the panel's native size at boot
(defvar *t4-native-on* nil) ; ... and whether it did (the Settings window shows it)
(defvar *t4-wc-pages* nil)  ; framebuffer pages made write-combining, or NIL

(defun %t4-native-panel ()
  "Switch an Intel GPU's pipe A to the panel's native size.  (W H) when it did, else NIL."
  (ignore-errors
   (multiple-value-bind (ew eh) (modus-int::%vbe-edid-size)
     (let* ((pcr (lambda (r) (modus-int::pci-config-read 0 2 0 r)))
            (id (funcall pcr 0)))
       (when (and ew eh (= (logand id #xFFFF) #x8086)
                  (not (and (= ew *t4-w*) (= eh *t4-h*))))
         (let* ((bar (logand (funcall pcr #x10) (lognot 15)))
                (va (modus-int::pc-mmio-map (+ bar #x60000) #x12000))
                (rd (lambda (o) (modus-int::%mmio-read32 (+ va (- o #x60000)))))
                (wr (lambda (o v) (modus-int::%mmio-write32 (+ va (- o #x60000)) v)))
                (htotal (funcall rd #x60000)) (vtotal (funcall rd #x6000C)))
           (when (and (= (1+ (logand htotal #xFFF)) ew) (= (1+ (logand vtotal #xFFF)) eh))
             (let ((pitch (* 64 (ceiling (* ew 4) 64)))
                   (phys (logand (mem-ref #x20000 :u32) (lognot #xFFF))))   ; the window's first page
               ;; Panel fitter off.  On Sandy Bridge a fitter change takes effect only when
               ;; PF_WIN_SZ is written, so CTL alone leaves the BIOS's 1024->1366 scaling live,
               ;; and the 1366-wide source then scans out as its first row repeated.
               (funcall wr #x68080 0)                                    ; PF_CTL
               (funcall wr #x68070 0)                                    ; PF_WIN_POS
               (funcall wr #x68074 0)                                    ; PF_WIN_SZ: arms the change
               (funcall wr #x6001C (logior (ash (1- ew) 16) (1- eh)))    ; PIPEASRC
               (funcall wr #x70188 pitch)                                ; DSPASTRIDE
               ;; DSPACNTR bit 14, trickle feed off: what i915 sets on Sandy Bridge (read back from
               ;; Linux on this T420).  Without it a 1366-wide plane scans out one repeated pixel.
               (funcall wr #x70180 (logior (funcall rd #x70180) #x4000))
               (funcall wr #x7019C (funcall rd #x7019C))                 ; DSPASURF: latch
               (modus-int::%vbe-map phys (* pitch eh))
               (setf *t4-w* ew *t4-h* eh *t4-pitch* pitch
                     (mem-ref #x1B064 :u32) pitch (mem-ref #x1B068 :u32) ew (mem-ref #x1B06C :u32) eh)
               (list ew eh)))))))))

(defun %t4-screen-down ()
  "Give the screen back to the framebuffer console."
  (when (plusp *t4-w*) (setf (mem-ref #x1B078 :u32) 1)))

(defun %t4-px-addr (x y) (+ *t4-base* (* y *t4-pitch*) (* x 4)))

(defun %t4-fill (x y w h rgb)
  (let ((x1 (min *t4-w* (+ x w))) (y1 (min *t4-h* (+ y h))))
    (loop for yy from (max 0 y) below y1
          do (let ((a (%t4-px-addr (max 0 x) yy)))
               (loop for xx from (max 0 x) below x1
                     do (setf (mem-ref a :u32) rgb) (setq a (+ a 4)))))))

;;; --- the native copy ---------------------------------------------------------------------------
;;; A pixel a time in Lisp was ~360 ns under QEMU (an AREF and a MEM-REF store, each a call):
;;; a 1280x780 screen most of a second.  This routine is a few instructions a pixel: ROWS x COLS
;;; tagged pixel words (glass's pixel vectors hold fixnums, value << 1) to raw 32-bit pixels,
;;; reading its arguments from a block at #x58000 (just above the mutex arena) --
;;;   +0 source address  +4 destination  +8 cols  +12 rows  +16 source stride  +20 dest pitch
;;; It is written into the native tier's code area (jit-i386-native.lisp, #x12000000) and called
;;; as the function at that address: a leaf that saves every register and answers fixnum 0.
;;; Measured under QEMU: 1M pixels in under 10 ms (one tick of the clock), from 360 ms.
;;;   pushal; movl $0x58000,%ebx; movl (%ebx),%esi; movl 4(%ebx),%edi; movl 12(%ebx),%ebp
;;;   row: testl %ebp,%ebp; jz done; pushl %esi; pushl %edi; movl 8(%ebx),%ecx
;;;   col: testl %ecx,%ecx; jz rowend; movl (%esi),%eax; shrl $1,%eax; movl %eax,(%edi)
;;;        addl $4,%esi; addl $4,%edi; decl %ecx; jmp col
;;;   rowend: popl %edi; popl %esi; addl 16(%ebx),%esi; addl 20(%ebx),%edi; decl %ebp; jmp row
;;;   done: popal; xorl %eax,%eax; ret
(defparameter *t4-blit-code*
  '(96 187 0 128 5 0 139 51 139 123 4 139 107 12 133 237 116 35 86 87 139 75 8 133 201 116 15
    139 6 209 232 137 7 131 198 4 131 199 4 73 235 237 95 94 3 115 16 3 123 20 77 235 217 97 49
    192 195))
(defvar *t4-blit* nil)          ; the routine, as a function; :LISP where there is no code area

(defun %t4-blit-fn ()
  (or *t4-blit*
      (setf *t4-blit*
            (let ((ptr (and (boundp 'modus-int::*n386-ptr*) modus-int::*n386-ptr*))
                  (end (and (boundp 'modus-int::*n386-end*) modus-int::*n386-end*)))
              (if (and (integerp ptr) (integerp end) (< (+ ptr 80) end))
                  (let ((a (logand (+ ptr 15) (lognot 15))) (i 0))
                    (dolist (b *t4-blit-code*) (setf (mem-ref (+ a i) :u8) b) (setq i (+ i 1)))
                    (setf modus-int::*n386-ptr* (+ a 64))
                    (modus-int::%word->val (logior a 3)))
                  :lisp)))))

(defun %t4-copy (px pw sx sy cols rows dx dy)
  "Pixels (SX,SY)..+COLS,ROWS of pixel vector PX (row length PW) to the screen at (DX,DY).
   Nothing between taking PX's address and the copy allocates, so it cannot move."
  (let ((f (%t4-blit-fn)))
    (if (eq f :lisp)
        (dotimes (r rows)
          (let ((src (+ (* (+ sy r) pw) sx)) (dst (%t4-px-addr dx (+ dy r))))
            (dotimes (c cols)
              (setf (mem-ref dst :u32) (aref px src))
              (setq src (+ src 1) dst (+ dst 4)))))
        (progn
          (setf (mem-ref #x58000 :u32) (+ (- (modus-int::%val->word px) 5) (* 4 (+ (* sy pw) sx)))
                (mem-ref #x58004 :u32) (%t4-px-addr dx dy)
                (mem-ref #x58008 :u32) cols
                (mem-ref #x5800C :u32) rows
                (mem-ref #x58010 :u32) (* 4 pw)
                (mem-ref #x58014 :u32) *t4-pitch*)
          (funcall f)))
    nil))

;;; --- write-combining for the framebuffer --------------------------------------------------------
;;; The BIOS leaves the graphics aperture uncached, so every 4-byte store to the screen is its own
;;; bus transaction: measured on the T420, 17.5 ms to show a 600x400 drag frame (~70 ns a pixel).
;;; Write-combining gathers them into 64-byte bursts: below the clock's 0.5 ms resolution.
;;; PAT entry 1 (selected by a PTE with only PWT set) becomes WC, and the window's PTEs set PWT.
;;;   pushal; movl $0x277,%ecx; rdmsr; andl $0xFFFF00FF,%eax; orl $0x100,%eax; wrmsr; wbinvd;
;;;   movl %cr3,%eax; movl %eax,%cr3; popal; xorl %eax,%eax; ret
(defparameter *t4-wc-code*
  '(#x60 #xB9 #x77 #x02 #x00 #x00 #x0F #x32 #x25 #xFF #x00 #xFF #xFF #x0D #x00 #x01 #x00 #x00
    #x0F #x30 #x0F #x09 #x0F #x20 #xD8 #x0F #x22 #xD8 #x61 #x31 #xC0 #xC3))

(defun %t4-install-code (bytes)
  "BYTES into the native tier's code area, as a function; NIL where there is no code area."
  (let ((ptr (and (boundp 'modus-int::*n386-ptr*) modus-int::*n386-ptr*))
        (end (and (boundp 'modus-int::*n386-end*) modus-int::*n386-end*)))
    (when (and (integerp ptr) (integerp end) (< (+ ptr (length bytes) 32) end))
      (let ((a (logand (+ ptr 15) (lognot 15))) (i 0))
        (dolist (b bytes) (setf (mem-ref (+ a i) :u8) b) (setq i (+ i 1)))
        (setf modus-int::*n386-ptr* (+ a (* 16 (ceiling (length bytes) 16))))
        (modus-int::%word->val (logior a 3))))))

(defun %t4-write-combining ()
  "Make the framebuffer window (its page tables at #x20000) write-combining.  The page count."
  (ignore-errors
   (let ((f (%t4-install-code *t4-wc-code*)) (p0 (logand (mem-ref #x20000 :u32) (lognot #xFFF))) (n 0))
     (when f
       (loop for k from 0 below 4096
             do (let ((e (mem-ref (+ #x20000 (* 4 k)) :u32)))
                  (if (and (logbitp 0 e) (<= p0 (logand e (lognot #xFFF)))
                           (< (logand e (lognot #xFFF)) (+ p0 #x1000000)))
                      (progn (setf (mem-ref (+ #x20000 (* 4 k)) :u32) (logior e 8)) (setq n (+ n 1)))
                      (return))))
       (funcall f)                    ; PAT, then WBINVD and a CR3 reload put the PTEs in force
       n))))

;;; --- the pointer -----------------------------------------------------------------------------
;;; Drawn over the screen, with the pixels under it kept and put back when it moves.  The
;;; framebuffer is uncached, so the pixels under it are not read back from it: they are kept
;;; as the desk blits them (%T4-SHADOW-ROW), and the pointer is repainted after every blit.

(defparameter *t4-arrow*
  '("2          " "22         " "212        " "2112       " "21112      "
    "211112     " "2111112    " "21111112   " "211111112  " "2111111112 "
    "21111122222" "2112112    " "212 2112   " "22  2112   " "2    2112  " "     222   "))
(defvar *t4-px* 100)
(defvar *t4-py* 100)
(defvar *t4-under* nil)
(defvar *t4-under-x* 0)
(defvar *t4-under-y* 0)
(defvar *t4-under-pixels* (make-array 176 :initial-element 0))

(defun %t4-desk-fb ()
  "The desk's screen framebuffer, whichever desktop app.lisp made (glass.wm or glass.desk):
   through its %KILN-DESK-FB.  GLASS.DESK:DESK-FB on a glass.wm desk reads the wrong slot -- the
   window list -- and every pointer event failed on it."
  (let ((d (and (boundp '*kiln-desk*) (symbol-value '*kiln-desk*))))   ; app.lisp's desk
    (and d (if (fboundp '%kiln-desk-fb) (funcall '%kiln-desk-fb d) (glass.desk:desk-fb d)))))

(defun %t4-desk-pixel (x y)
  "What the desk drew at screen (X, Y): the pointer's background."
  (let ((fb (%t4-desk-fb)))
    (if (and fb (< -1 x (glass:fb-width fb)) (< -1 y (glass:fb-height fb)))
        (glass:fb-get fb x y)
        #x1E2530)))

(defvar *t4-hw-pointer* nil
  "A function of (X Y) that moves a hardware cursor (t420-gpu.lisp T4G-CURSOR-MOVE), or NIL for
   the pointer drawn into the screen here.")

(defun %t4-pointer-hide ()
  "Put back what the desk drew under the pointer: its 11 x 16 patch, copied from the desk's
   framebuffer (the screen's own pixels are never read back).  Nothing with a hardware cursor."
  (when *t4-under*
    (let ((fb (%t4-desk-fb)))
      (if fb
          (let* ((fw (glass:fb-width fb)) (fh (glass:fb-height fb))
                 (x (max 0 *t4-under-x*)) (y (max 0 *t4-under-y*))
                 (cols (- (min (+ *t4-under-x* 11) fw *t4-w*) x))
                 (rows (- (min (+ *t4-under-y* 16) fh *t4-h*) y)))
            (when (and (> cols 0) (> rows 0))
              (%t4-copy (glass:fb-pixels fb) fw x y cols rows x y)))
          (let ((x *t4-under-x*) (y *t4-under-y*))
            (dotimes (r 16)
              (dotimes (c 11)
                (let ((sx (+ x c)) (sy (+ y r)))
                  (when (and (< -1 sx *t4-w*) (< -1 sy *t4-h*))
                    (setf (mem-ref (%t4-px-addr sx sy) :u32) (%t4-desk-pixel sx sy)))))))))
    (setf *t4-under* nil)))

(defun %t4-pointer-show ()
  (when *t4-hw-pointer*                     ; the display engine draws it: one register write
    (funcall *t4-hw-pointer* *t4-px* *t4-py*)
    (return-from %t4-pointer-show nil))
  (let ((x *t4-px*) (y *t4-py*))
    (dotimes (r 16)
      (let ((line (nth r *t4-arrow*)))
        (dotimes (c 11)
          (let ((sx (+ x c)) (sy (+ y r)))
            (when (and (< -1 sx *t4-w*) (< -1 sy *t4-h*))
              (case (char line c)
                (#\2 (setf (mem-ref (%t4-px-addr sx sy) :u32) #x000000))
                (#\1 (setf (mem-ref (%t4-px-addr sx sy) :u32) #xffffff))))))))
    (setf *t4-under-x* x *t4-under-y* y *t4-under* t)))

;;; --- the 8042: keyboard and aux (mouse) ------------------------------------------------------

(defun %t4-in () (modus-int::io-in-byte #x60))
(defun %t4-status () (modus-int::io-in-byte #x64))

(defun %t4-wait-write ()
  "Until the controller's input buffer is empty (bit 1), bounded."
  (dotimes (i 100000) (when (zerop (logand (%t4-status) 2)) (return t))))

(defun %t4-wait-read ()
  "Until a byte is waiting (bit 0), bounded.  T when one is."
  (dotimes (i 100000) (when (logbitp 0 (%t4-status)) (return t))))

(defun %t4-cmd (c) (%t4-wait-write) (modus-int::io-out-byte #x64 c))
(defun %t4-data (d) (%t4-wait-write) (modus-int::io-out-byte #x60 d))

(defun %t4-drain ()
  "Throw away whatever the controller holds (a mouse's stray packet, a key's break code)."
  (dotimes (i 64) (if (logbitp 0 (%t4-status)) (%t4-in) (return))))

(defun %t4-mouse-send (b)
  "Send B to the aux device; its answer -- #xFA (ACK), #xFE (resend), #xFC (error) -- or NIL.
   Bytes that are none of those (a packet already in flight) are skipped."
  (%t4-cmd #xD4) (%t4-data b)
  (dotimes (i 8 nil)
    (unless (%t4-wait-read) (return nil))
    (let ((r (%t4-in))) (when (member r '(#xFA #xFE #xFC)) (return r)))))

(defvar *t4-mouse-ok* nil)

(defun %t4-mouse-init ()
  "Enable the aux port and its device in plain 3-byte PS/2 mode, polled (no IRQ 12).  On a
   docked T420 the port is shared by the TrackPoint, the touchpad and the dock's mouse port
   (and a USB mouse, by the BIOS's emulation), any of which may be mid-packet or slow to
   answer -- so the controller is drained first and each step is tried three times.  The aux
   port is polled whatever the answers: *T4-MOUSE-OK* only says whether they came."
  (%t4-drain)
  (%t4-cmd #xA8)                                                      ; enable the aux port
  (%t4-cmd #x20)                                                      ; read the config byte
  (let ((cfg (if (%t4-wait-read) (%t4-in) 0)))
    ;; clear bit 5 (aux clock disabled) and both IRQ enables: this layer polls
    (%t4-cmd #x60) (%t4-data (logand cfg (lognot (logior #x20 #x02 #x01)))))
  (let ((ok nil))
    (dotimes (try 3)
      (%t4-drain)
      (when (and (eql (%t4-mouse-send #xF6) #xFA)                    ; defaults
                 (eql (%t4-mouse-send #xF4) #xFA))                   ; report
        (setq ok t) (return)))
    (setf *t4-mouse-ok* ok))
  (%t4-drain)
  (setf *t4-pkt-n* 0)
  *t4-mouse-ok*)

;;; keyboard: scan code set 1, as the 8042 translates it.  A table of (code . (plain shifted)).
(defparameter *t4-keys*
  (let ((v (make-array 128 :initial-element nil)))
    (loop for (code plain shifted)
            in '((2 #\1 #\!) (3 #\2 #\@) (4 #\3 #\#) (5 #\4 #\$) (6 #\5 #\%) (7 #\6 #\^)
                 (8 #\7 #\&) (9 #\8 #\*) (10 #\9 #\() (11 #\0 #\)) (12 #\- #\_) (13 #\= #\+)
                 (16 #\q #\Q) (17 #\w #\W) (18 #\e #\E) (19 #\r #\R) (20 #\t #\T) (21 #\y #\Y)
                 (22 #\u #\U) (23 #\i #\I) (24 #\o #\O) (25 #\p #\P) (26 #\[ #\{) (27 #\] #\})
                 (30 #\a #\A) (31 #\s #\S) (32 #\d #\D) (33 #\f #\F) (34 #\g #\G) (35 #\h #\H)
                 (36 #\j #\J) (37 #\k #\K) (38 #\l #\L) (39 #\; #\:) (40 #\' #\") (41 #\` #\~)
                 (43 #\\ #\|) (44 #\z #\Z) (45 #\x #\X) (46 #\c #\C) (47 #\v #\V) (48 #\b #\B)
                 (49 #\n #\N) (50 #\m #\M) (51 #\, #\<) (52 #\. #\>) (53 #\/ #\?) (57 #\Space #\Space))
          do (setf (svref v code) (cons (char-code plain) (char-code shifted))))
    v))

(defvar *t4-shift* nil)
(defvar *t4-ctrl* nil)
(defvar *t4-e0* nil)

(defun %t4-key-keysym (code)
  "The X11 keysym for a set-1 make code (after an E0 prefix when *T4-E0*), or NIL."
  (if *t4-e0*
      (case code
        (#x48 #xff52) (#x50 #xff54) (#x4B #xff51) (#x4D #xff53)        ; arrows
        (#x47 #xff50) (#x4F #xff57) (#x49 #xff55) (#x51 #xff56)        ; home end pgup pgdn
        (#x53 #xffff) (#x52 #xff63) (#x1C #xff0d) (#x35 #x2f)          ; del ins kp-enter kp-/
        (t nil))
      (case code
        (#x1C #xff0d) (#x0E #xff08) (#x0F #xff09) (#x01 #xff1b)        ; ret bs tab esc
        ((#x3B #x3C #x3D #x3E #x3F #x40 #x41 #x42 #x43 #x44) (+ #xffbe (- code #x3B)))  ; F1-F10
        (#x57 #xffc8) (#x58 #xffc9)                                    ; F11 F12
        (t (let ((e (and (< code 128) (svref *t4-keys* code))))
             (when e
               (let ((c (if *t4-shift* (cdr e) (car e))))
                 ;; Ctrl and a letter: the control code, as the Zero passes it
                 (if (and *t4-ctrl* (<= 97 (car e) 122)) (- (car e) 96) c))))))))

(defun %t4-key-byte (b)
  "A byte from the keyboard: an event, or NIL."
  (cond ((= b #xE0) (setf *t4-e0* t) nil)
        (t (let ((e0 *t4-e0*) (up (>= b 128)) (code (logand b 127)))
             (setf *t4-e0* nil)
             (cond ((member code '(#x2A #x36)) (unless e0 (setf *t4-shift* (not up))) nil)
                   ((= code #x1D) (setf *t4-ctrl* (not up)) nil)                   ; either Ctrl
                   ((= code #x38) nil)                                             ; Alt
                   (up nil)
                   (t (let* ((*t4-e0* e0) (ks (%t4-key-keysym code)))
                        (and ks (+ (* 4 1099511627776) ks)))))))))

;;; aux: 3-byte packets; byte 0 has bit 3 set (resync on it), X/Y sign in bits 4/5
(defvar *t4-pkt* (list 0 0 0))
(defvar *t4-pkt-n* 0)
(defvar *t4-buttons* 0)

(defun %t4-event (type x y) (+ (* type 1099511627776) (* y 1048576) x))

(defun %t4-mouse-byte (b)
  "A byte from the aux port: an event when it completes a packet, else NIL."
  (when (and (= *t4-pkt-n* 0) (not (logbitp 3 b)))
    (return-from %t4-mouse-byte nil))                                 ; out of step: wait for a header
  (setf (nth *t4-pkt-n* *t4-pkt*) b *t4-pkt-n* (+ *t4-pkt-n* 1))
  (when (= *t4-pkt-n* 3)
    (setf *t4-pkt-n* 0)
    (let* ((h (first *t4-pkt*))
           (dx (if (logbitp 4 h) (- (second *t4-pkt*) 256) (second *t4-pkt*)))
           (dy (if (logbitp 5 h) (- (third *t4-pkt*) 256) (third *t4-pkt*)))
           ;; X11's mask, as app.lisp's type-12 event carries it: PS/2 has left, right, middle
           ;; in bits 0-2; X11 counts 1 left, 2 middle, 4 right (glass.wm's window menu is 4)
           (nb (logior (logand h 1) (if (logbitp 2 h) 2 0) (if (logbitp 1 h) 4 0))))
      (unless (or (logbitp 6 h) (logbitp 7 h))                         ; overflow: drop it
        (setf *t4-px* (max 0 (min (- *t4-w* 1) (+ *t4-px* dx)))
              *t4-py* (max 0 (min (- *t4-h* 1) (- *t4-py* dy))))      ; PS/2 Y grows upward
        (setf *t4-buttons* nb)
        (%t4-pointer-hide) (%t4-pointer-show)
        ;; type 12: a pointer with all its buttons (bits 48 up), every packet -- a press, a
        ;; release, a drag and a hover alike; the desk works out which from the mask
        (+ (* nb 281474976710656) (%t4-event 12 *t4-px* *t4-py*))))))

(defvar *t4-events* nil)

(defun %t4-poll ()
  "Everything the 8042 holds, and what the USB keyboard and mouse reported (t420-usb.lisp), as
   queued events.  Bounded per call."
  (when (fboundp 't4-usb-poll) (funcall 't4-usb-poll))
  (dotimes (i 64)
    (let ((s (%t4-status)))
      (unless (logbitp 0 s) (return))
      (let* ((b (%t4-in)) (e (if (logbitp 5 s) (%t4-mouse-byte b) (%t4-key-byte b))))
        (when e (setf *t4-events* (nconc *t4-events* (list e))))))))

(defun %t4-next-event ()
  (unless *t4-events* (%t4-poll))
  (if *t4-events* (pop *t4-events*) 0))

(defun %kiln-frame-wait (ms)
  "Sleep out the rest of a frame, polling the keyboard and mouse meanwhile; the log goes to
   the disk from here when it has news."
  (%t4-log-idle)
  (let ((end (+ (get-internal-real-time) (* ms (floor internal-time-units-per-second 1000)))))
    (loop (%t4-poll)
          (when (or *t4-events* (>= (get-internal-real-time) end)) (return)))))

;;; --- the log on the boot disk --------------------------------------------------------------------
;;; A PC like the T420 has no serial port, and a camera cannot read 14-pixel text, so what the
;;; console prints from the desk's start (DHCP, the NIC's diagnostics, errors: pc-console.lisp's
;;; console ring) and what the desk says (T4-LOG) go to a RAM log, written to the boot disk's last 2 MB through the BIOS (INT 13h AH=43h, the pcboot
;;; shim's PM_BIOS) when the desk is idle.  On the rig the disk is the Pi Zero's USB gadget, so
;;; the log is read straight out of its image file; on a stick, out of the stick:
;;;   LBA +0: "T4LOG" then the byte count (u32 at +8) and the flush count (+12); data from LBA +1.
;;; `kiln t420' pads the disk so the region exists (*T4-LOG-LBA*, 4096 sectors).

(defvar *t4-log-lba* 262144)        ; kiln t420 --log-lba sets it in the core
(defvar *t4-log* (make-array (* 1024 1024) :element-type '(unsigned-byte 8)))
(defvar *t4-log-n* 0)           ; bytes in the log
(defvar *t4-log-flushed* 0)     ; bytes on the disk
(defvar *t4-log-flushes* 0)
(defvar *t4-log-at* 0)          ; GET-INTERNAL-REAL-TIME of the last flush

(defun t4-log-byte (b)
  (when (< *t4-log-n* (length *t4-log*))
    (setf (aref *t4-log* *t4-log-n*) b *t4-log-n* (+ *t4-log-n* 1))))

(defvar *t4-log-udp* (cons #x0A000001 7778)
  "(ip . port): every log line also goes there as a UDP datagram, once the network is up -- a log
   that does not need the disk (the rig's Pi, the gateway, listens: nc -u -l -k 7778).  NIL: off.")

(defun %t4-log-udp (s)
  (when (and *t4-log-udp* (boundp '*t4-ip*) (consp (symbol-value '*t4-ip*))
             (integerp (first (symbol-value '*t4-ip*))) (fboundp 'modus-int::udp-send))
    (ignore-errors
      (let* ((n (min 1400 (+ 1 (length s)))) (b (make-array n :initial-element 10)))
        (dotimes (i (- n 1)) (setf (aref b i) (logand (char-code (char s i)) 255)))
        (funcall 'modus-int::udp-send (car *t4-log-udp*) (cdr *t4-log-udp*) (cdr *t4-log-udp*) b n)))))

(defun t4-log (fmt &rest args)
  "Append a line to the disk log, as FORMAT would print it (and send it, %T4-LOG-UDP)."
  (let ((s (apply #'format nil fmt args)))
    (dotimes (i (length s)) (t4-log-byte (logand (char-code (char s i)) 255)))
    (t4-log-byte 10)
    (%t4-log-udp s)
    nil))

(defvar *t4-disk-own* nil
  "(read . write): the desk's own driver for the boot disk (t420-usb-disk.lisp), once it has
   taken the disk's controller from the BIOS; NIL: INT 13h.")

(defun %t4-disk-write (lba nsec)
  "Write NSEC (<= 64) sectors from #x30000 to LBA of the boot disk.  T when it worked."
  (when *t4-disk-own* (return-from %t4-disk-write (funcall (cdr *t4-disk-own*) lba nsec)))
  (setf (mem-ref #xC000 :u8) 16 (mem-ref #xC001 :u8) 0
        (mem-ref #xC002 :u8) nsec (mem-ref #xC003 :u8) 0
        (mem-ref #xC004 :u32) 0                       ; buffer offset 0 ...
        (mem-ref #xC006 :u8) #x00 (mem-ref #xC007 :u8) #x30   ; ... segment 3000h
        (mem-ref #xC008 :u32) lba (mem-ref #xC00C :u32) 0)
  (and (modus-int::%pcbios-int #x13 #x4300 0 0 (logand (mem-ref #x58C :u32) #xFF) #xC000 0 0)
       (zerop (logand (modus-int::%pcbios-out 40) 1))))   ; carry clear

(defun t4-log-flush ()
  "Write what is new in the log (whole sectors from the first unwritten one), then the header."
  (%t4-ring-drain)
  (when (and (> *t4-log-n* *t4-log-flushed*) (modus-int::%pcbios-present-p))
    (let* ((first (floor *t4-log-flushed* 512)) (last (ceiling *t4-log-n* 512)) (s first))
      (loop while (< s last)
            do (let ((n (min 64 (- last s))))
                 (dotimes (i (* n 512))
                   (let ((k (+ (* s 512) i)))
                     (setf (mem-ref (+ #x30000 i) :u8) (if (< k *t4-log-n*) (aref *t4-log* k) 0))))
                 (%t4-disk-write (+ *t4-log-lba* 1 s) n)
                 (setq s (+ s n))))
      (setq *t4-log-flushes* (+ *t4-log-flushes* 1))
      (dotimes (i 512) (setf (mem-ref (+ #x30000 i) :u8) 0))
      (let ((m "T4LOG")) (dotimes (i 5) (setf (mem-ref (+ #x30000 i) :u8) (char-code (char m i)))))
      (setf (mem-ref #x30008 :u32) *t4-log-n* (mem-ref #x3000C :u32) *t4-log-flushes*)
      (%t4-disk-write *t4-log-lba* 1)
      (setq *t4-log-flushed* *t4-log-n* *t4-log-at* (get-internal-real-time))))
  *t4-log-n*)

;;; --- the disk command channel ---------------------------------------------------------------
;;; Lisp to evaluate, without the keyboard or the screen: the rig's Zero writes it into the disk
;;; image (scripts on the Zero: t4cmd.py) and the desk's idle loop picks it up and logs the answer.
;;;   LBA *T4-CMD-LBA*: "T4CMD" then a sequence number (u32 at +8) and the text's length (+12);
;;;   the text from +16, up to 15 sectors in all.  A new sequence number is a new command.
(defvar *t4-cmd-lba* (+ 262144 2560)) ; inside the log's region, past what a 1 MB log can reach
(defvar *t4-cmd-seq* nil)       ; the last one seen (NIL: none yet -- the first read only records it)
(defvar *t4-cmd-at* 0)

(defun %t4-disk-read (lba nsec)
  "Read NSEC (<= 64) sectors at LBA of the boot disk to #x30000.  T when it worked."
  (when *t4-disk-own* (return-from %t4-disk-read (funcall (car *t4-disk-own*) lba nsec)))
  (setf (mem-ref #xC000 :u8) 16 (mem-ref #xC001 :u8) 0
        (mem-ref #xC002 :u8) nsec (mem-ref #xC003 :u8) 0
        (mem-ref #xC004 :u32) 0
        (mem-ref #xC006 :u8) #x00 (mem-ref #xC007 :u8) #x30
        (mem-ref #xC008 :u32) lba (mem-ref #xC00C :u32) 0)
  (and (modus-int::%pcbios-int #x13 #x4200 0 0 (logand (mem-ref #x58C :u32) #xFF) #xC000 0 0)
       (zerop (logand (modus-int::%pcbios-out 40) 1))))

(defun t4-cmd-baseline ()
  "Take the command channel's current sequence number as already done, at the desk's start: a
   command written after this is new.  (Taken at the first idle poll instead, a command sent
   while the desk was still starting was taken for the baseline and never run.)"
  (when (and (modus-int::%pcbios-present-p) (%t4-disk-read *t4-cmd-lba* 1)
             (= (mem-ref #x30000 :u8) 84) (= (mem-ref #x30004 :u8) 68))
    (setq *t4-cmd-seq* (mem-ref #x30008 :u32)))
  (t4-log "kiln t420: commands after ~D" (or *t4-cmd-seq* 0))
  (unless *t4-cmd-seq* (setq *t4-cmd-seq* 0)))

(defun %t4-cmd-poll ()
  "Run the command in the command sectors if it is new (at most once a second)."
  (when (and (modus-int::%pcbios-present-p)
             (> (- (get-internal-real-time) *t4-cmd-at*) internal-time-units-per-second))
    (setq *t4-cmd-at* (get-internal-real-time))
    (when (and (%t4-disk-read *t4-cmd-lba* 16)
               (= (mem-ref #x30000 :u8) 84) (= (mem-ref #x30004 :u8) 68))       ; "T...D"
      (let ((seq (mem-ref #x30008 :u32)) (n (min (mem-ref #x3000C :u32) (- (* 16 512) 16))))
        (cond ((null *t4-cmd-seq*) (setq *t4-cmd-seq* seq))
              ((/= seq *t4-cmd-seq*)
               (setq *t4-cmd-seq* seq)
               (let ((text (make-string n)))
                 (dotimes (i n) (setf (char text i) (code-char (mem-ref (+ #x30010 i) :u8))))
                 (handler-case
                     (t4-log "cmd ~d => ~s" seq (eval (read-from-string text)))
                   (error (c) (t4-log "cmd ~d error: ~a" seq c))))
               (t4-log-flush)))))))

;;; --- the GPU's registers, for comparing with what Linux sets ---------------------------------
(defvar *t4-gpu-va* nil)          ; the first 1 MB of BAR0, mapped once

(defun %t4-gpu-va ()
  (or *t4-gpu-va*
      (setq *t4-gpu-va*
            (modus-int::pc-mmio-map (logand (modus-int::pci-config-read 0 2 0 #x10) (lognot 15))
                                    #x100000))))
(defun t4-gpu-reg (off) (modus-int::%mmio-read32 (+ (%t4-gpu-va) off)))
(defun t4-gpu-set (off v) (modus-int::%mmio-write32 (+ (%t4-gpu-va) off) v))

(defparameter *t4-gpu-ranges*
  '((#x42000 #x100) (#x44000 #x40) (#x45000 #x200) (#x46000 #x100) (#x48000 #x300) (#x4F000 #x100)
    (#x60000 #x200) (#x64000 #x100) (#x68000 #x100) (#x70000 #x200) (#xC6000 #x100)
    (#xC7000 #x300) (#xE0000 #x100) (#xE1100 #x100) (#xF0000 #x100)))

(defun t4-gpu-dump (label)
  "Log every non-zero register in *T4-GPU-RANGES*, as `LABEL OFFSET VALUE' lines (hex)."
  (ignore-errors
   (when (= (logand (modus-int::pci-config-read 0 2 0 0) #xFFFF) #x8086)
     (dolist (r *t4-gpu-ranges*)
       (loop for o from (first r) below (+ (first r) (second r)) by 4
             do (let ((v (t4-gpu-reg o)))
                  (unless (zerop v) (t4-log "~a ~5,'0x ~8,'0x" label o v)))))
     (t4-log-flush))))

(defvar *t4-idle-seen* nil)

(defun %t4-log-idle ()
  "From the desk's idle wait: run a new disk command; flush, at most every 2 s."
  (unless *t4-idle-seen*                 ; the desk's start, profiled (t420-desk.lisp %T4-PHASE)
    (setq *t4-idle-seen* t)
    (when (fboundp '%t4-phase) (funcall '%t4-phase "desk up (first idle)")))
  (%t4-ring-drain)
  (ignore-errors (%t4-cmd-poll))
  (when (and (> *t4-log-n* *t4-log-flushed*)
             (> (- (get-internal-real-time) *t4-log-at*) (* 2 internal-time-units-per-second)))
    (t4-log-flush)))

(defvar *t4-ring-read* 0)        ; how much of the console ring the log has taken

(defun %t4-console-to-log (on)
  "While ON, what the console prints is kept: pc-console.lisp's console ring (64 KB at #x60000,
   count at #x5FFF0) is switched on here, and drained into the log at each flush.  The console
   itself stays on VGA text memory (#x1B078 = 2), which a graphics mode does not show."
  (if on
      (setf (mem-ref #x5FFF0 :u32) 0 *t4-ring-read* 0 (mem-ref #x5FFF4 :u32) #x1234567)
      (setf (mem-ref #x5FFF4 :u32) 0))
  (when (plusp *t4-w*) (setf (mem-ref #x1B078 :u32) (if on 2 1))))

(defun %t4-ring-drain ()
  (let ((n (min (mem-ref #x5FFF0 :u32) 65536)))
    (loop while (< *t4-ring-read* n)
          do (t4-log-byte (mem-ref (+ #x60000 *t4-ring-read*) :u8))
             (setq *t4-ring-read* (+ *t4-ring-read* 1)))))

;;; --- the date -------------------------------------------------------------------------------
;;; Modus counts GET-UNIVERSAL-TIME from boot until something sets it (SET-UNIVERSAL-TIME).  A PC
;;; has a battery clock, the CMOS RTC, which the BIOS keeps in LOCAL time (the Windows habit) --
;;; read at boot, so the clock is right before the network answers -- and SNTP sets it properly
;;; once the network is up.  The desk shows *KILN-TIME-ZONE* (app.lisp's clock): hours west of
;;; Greenwich, as CL counts them.  5: the rig's own machines run at UTC-5.

(defvar *kiln-time-zone* 5)
(defvar *t4-clock-source* nil)        ; :sntp, :rtc or NIL

(defun %t4-kfn (name)
  "The kernel function NAME (a string), from whichever kernel package holds it, or NIL."
  (dolist (p '("MODUS-INT" "MODUS.NET" "MODUS.MVM" "MODUS.RUNTIME" "COMMON-LISP-USER") nil)
    ;; INTERN, not FIND-SYMBOL: a baked kernel function's symbol exists only once something has
    ;; read its name (as the REPL does for modus-int::udp-send), and the call resolves by name.
    (let ((sym (and (find-package p)
                    (if (string= p "MODUS-INT") (intern name p) (find-symbol name p)))))
      (when (and sym (fboundp sym)) (return (symbol-function sym))))))

(defun %t4-cmos (reg)
  (modus-int::io-out-byte #x70 reg)
  (modus-int::io-in-byte #x71))

(defvar *t4-rtc-zone* 0
  "The zone the CMOS clock keeps, in CL's hours west: 0, UTC -- what this T420's held (it read
   23:47 at 18:47 in UTC-5).  A machine whose clock Windows set keeps local time: *KILN-TIME-ZONE*.")

(defun t4-rtc-time ()
  "The CMOS clock's time as universal time (the RTC taken as *T4-RTC-ZONE*), or NIL."
  (dotimes (i 100000) (unless (logbitp 7 (%t4-cmos #x0A)) (return)))   ; not mid-update
  (let* ((b (%t4-cmos #x0B))
         (bcd (not (logbitp 2 b))) (h24 (logbitp 1 b))
         (conv (lambda (v) (if bcd (+ (* 10 (ash v -4)) (logand v 15)) v)))
         (sec (funcall conv (%t4-cmos #x00)))
         (min (funcall conv (%t4-cmos #x02)))
         (hr-raw (%t4-cmos #x04))
         (hr (let ((h (funcall conv (logand hr-raw #x7F))))
               (if h24 h (+ (mod h 12) (if (logbitp 7 hr-raw) 12 0)))))
         (day (funcall conv (%t4-cmos #x07)))
         (mon (funcall conv (%t4-cmos #x08)))
         (yr (+ 2000 (funcall conv (%t4-cmos #x09)))))
    (when (and (<= 2020 yr 2099) (<= 1 mon 12) (<= 1 day 31) (< hr 24) (< min 60) (< sec 60))
      (encode-universal-time sec min hr day mon yr *t4-rtc-zone*))))

(defun t4-sntp (&key (server "pool.ntp.org") (tries 3))
  "Universal time from SNTP (RFC 4330) over modus's own UDP (net/ip.lisp), or NIL.  Needs the
   network up (KILN-T420-MAIN brings it up first)."
  (let ((dns (%t4-kfn "DNS-RESOLVE")) (send (%t4-kfn "UDP-SEND")) (recv (%t4-kfn "UDP-RECEIVE"))
        (ref (%t4-kfn "UDP-RX-DATA-REF")) (base (%t4-kfn "E1000-STATE-BASE")))
    (unless (and dns send recv ref base)
      (t4-log "sntp: missing ~S" (list :dns dns :send send :recv recv :ref ref :base base))
      (return-from t4-sntp nil))
    (let* ((n (length server)) (name (make-array n)))
      (dotimes (i n) (setf (aref name i) (char-code (char server i))))
      ;; the first lookup after DHCP can go unanswered (one boot logged DNS:F): try again
      (let ((ip (dotimes (k 3 nil)
                  (let ((a (ignore-errors (funcall dns name n))))
                    (when (and a (not (eql a 0))) (return a))
                    (modus-int::%sleep-ms 500)))))
        (if (or (null ip) (zerop ip))
            (progn (t4-log "sntp: no address for ~A" server) nil)
            (let ((q (make-array 48 :initial-element 0)))
              (setf (aref q 0) #x23)                               ; LI 0, VN 4, mode 3
              (dotimes (k tries (progn (t4-log "sntp: no answer from ~A" server) nil))
                (funcall send ip 123 12399 q 48)
                (let ((got (funcall recv 2000)))
                  (when (and got (>= got 48)
                             (= (mem-ref (+ (funcall base) #x74) :u16) 123))   ; from port 123
                    (let ((ut (logior (ash (funcall ref 40) 24) (ash (funcall ref 41) 16)
                                      (ash (funcall ref 42) 8) (funcall ref 43))))
                      (when (> ut 3900000000) (return ut))))))))))))

;;; The bare i386 image has no SET-UNIVERSAL-TIME (its GET-UNIVERSAL-TIME answers 0: no RTC is
;;; read).  GET-UNIVERSAL-TIME is a CL symbol, so redefining it reaches every caller compiled at
;;; run time -- the desk's clock among them: UT now, counted on by GET-INTERNAL-REAL-TIME.
(defun %t4-set-ut (ut)
  (let ((base (- ut (floor (get-internal-real-time) internal-time-units-per-second))))
    (setf (fdefinition 'get-universal-time)
          (lambda () (+ base (floor (get-internal-real-time) internal-time-units-per-second))))
    ut))

(defun t4-set-clock (&optional network-up)
  "Set the date: SNTP when the network is up, else the CMOS clock.  The source, or NIL."
  (let ((set (%t4-kfn "SET-UNIVERSAL-TIME"))
        (ut nil) (src nil))
    (when network-up (setq ut (ignore-errors (t4-sntp)) src :sntp))
    (unless ut (setq ut (ignore-errors (t4-rtc-time)) src :rtc))
    (cond ((null ut) (t4-log "clock: neither SNTP nor the RTC answered") nil)
          (t (if set (funcall set ut) (%t4-set-ut ut))
             (setf *t4-clock-source* src)
             (multiple-value-bind (s m h d mo y) (decode-universal-time ut *kiln-time-zone*)
               (t4-log "clock: ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d (UTC-~d) from ~(~a~)"
                       y mo d h m s *kiln-time-zone* src))
             src))))

;;; --- app.lisp's platform calls ---------------------------------------------------------------

(defun %kiln-sys (n a b c &optional (d 0) (e 8))
  (declare (ignore d e))
  (case n
    (1001 (case a (0 *t4-w*) (1 *t4-h*) (t 1)))
    (1002 (%t4-pointer-hide) (%t4-fill 0 0 (logand b #xFFFF) (floor b 65536) c) (%t4-pointer-show) 0)
    (1004 (%t4-next-event))
    (1010 1)                                                          ; no speaker
    (t 0)))
