;;;; t420.lisp — kiln's desk on a PC running bare-metal modus on 32-bit x86: the ThinkPad
;;;; T420 first.  The desk on the VBE framebuffer, the PS/2 keyboard as keys and the PS/2 aux
;;;; port (the TrackPoint and the touchpad, which the firmware presents as one PS/2 mouse) as
;;;; the pointer.
;;;;
;;;; The platform layer for app.lisp, as zero.lisp is for the Pi Zero, over the bare i386
;;;; image's own drivers: kernel/net/pc-vbe.lisp (VBE through the pcboot shim's PM_BIOS, the
;;;; framebuffer mapped at #x0C000000) and plain port I/O for the 8042.  `kiln t420' installs
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

(defun %t4-desk-pixel (x y)
  "What the desk drew at screen (X, Y): the pointer's background."
  (let ((d (and (boundp '*kiln-desk*) (symbol-value '*kiln-desk*))))   ; app.lisp's desk
    (if (and d (< -1 x (glass:fb-width (glass.desk:desk-fb d))) (< -1 y (glass:fb-height (glass.desk:desk-fb d))))
        (glass:fb-get (glass.desk:desk-fb d) x y)
        #x1E2530)))

(defun %t4-pointer-hide ()
  "Put back what the desk drew under the pointer: its 11 x 16 patch, copied from the desk's
   framebuffer (the screen's own pixels are never read back)."
  (when *t4-under*
    (let* ((d (and (boundp '*kiln-desk*) (symbol-value '*kiln-desk*)))
           (fb (and d (glass.desk:desk-fb d))))
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

(defun %t4-mouse-send (b)
  "Send B to the aux device; its answer (#xFA, ACK) or NIL."
  (%t4-cmd #xD4) (%t4-data b)
  (and (%t4-wait-read) (%t4-in)))

(defvar *t4-mouse-ok* nil)

(defun %t4-mouse-init ()
  "Enable the aux port and its device, in plain 3-byte PS/2 mode, polled (no IRQ 12)."
  (dotimes (i 32) (if (logbitp 0 (%t4-status)) (%t4-in) (return)))   ; drain
  (%t4-cmd #xA8)                                                      ; enable the aux port
  (%t4-cmd #x20)                                                      ; read the config byte
  (let ((cfg (if (%t4-wait-read) (%t4-in) 0)))
    ;; clear bit 5 (aux clock disabled) and both IRQ enables: this layer polls
    (%t4-cmd #x60) (%t4-data (logand cfg (lognot (logior #x20 #x02 #x01)))))
  (let ((a (%t4-mouse-send #xF6)) (b (%t4-mouse-send #xF4)))         ; defaults, then report
    (setf *t4-mouse-ok* (and (eql a #xFA) (eql b #xFA))))
  (dotimes (i 32) (if (logbitp 0 (%t4-status)) (%t4-in) (return)))
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
           (nb (logand h 1)) (was *t4-buttons*))
      (unless (or (logbitp 6 h) (logbitp 7 h))                         ; overflow: drop it
        (setf *t4-px* (max 0 (min (- *t4-w* 1) (+ *t4-px* dx)))
              *t4-py* (max 0 (min (- *t4-h* 1) (- *t4-py* dy))))      ; PS/2 Y grows upward
        (setf *t4-buttons* nb)
        (%t4-pointer-hide) (%t4-pointer-show)
        (%t4-event (cond ((and (= nb 1) (= was 0)) 1) ((and (= nb 0) (= was 1)) 3)
                         ((= nb 1) 2) (t 5))
                   *t4-px* *t4-py*)))))

(defvar *t4-events* nil)

(defun %t4-poll ()
  "Everything the 8042 holds, as queued events.  Bounded per call."
  (dotimes (i 64)
    (let ((s (%t4-status)))
      (unless (logbitp 0 s) (return))
      (let* ((b (%t4-in)) (e (if (logbitp 5 s) (%t4-mouse-byte b) (%t4-key-byte b))))
        (when e (setf *t4-events* (nconc *t4-events* (list e))))))))

(defun %t4-next-event ()
  (unless *t4-events* (%t4-poll))
  (if *t4-events* (pop *t4-events*) 0))

(defun %kiln-frame-wait (ms)
  "Sleep out the rest of a frame, polling the keyboard and mouse meanwhile."
  (let ((end (+ (get-internal-real-time) (* ms (floor internal-time-units-per-second 1000)))))
    (loop (%t4-poll)
          (when (or *t4-events* (>= (get-internal-real-time) end)) (return)))))

;;; --- app.lisp's platform calls ---------------------------------------------------------------

(defun %kiln-sys (n a b c &optional (d 0) (e 8))
  (declare (ignore d e))
  (case n
    (1001 (case a (0 *t4-w*) (1 *t4-h*) (t 1)))
    (1002 (%t4-pointer-hide) (%t4-fill 0 0 (logand b #xFFFF) (floor b 65536) c) (%t4-pointer-show) 0)
    (1004 (%t4-next-event))
    (1010 1)                                                          ; no speaker
    (t 0)))
