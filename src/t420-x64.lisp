;;;; t420-x64.lisp -- the 64-bit T420 image's start, until the desk itself runs on x64.
;;;;
;;;; The x64 image boots through UEFI from the rig's disk and restores a core the build saved
;;;; under QEMU (\CORE.BIN on the ESP; boot/boot-uefi-x64.lisp).  The core's restart hook is
;;;; T420-X64-MAIN: it takes the boot disk's controller (t420-usb-disk.lisp: there is no BIOS
;;;; after ExitBootServices, so the desk's own driver is the only way to the disk), starts the
;;;; log and the disk command channel (t420.lisp), and serves the channel until a key arrives
;;;; on the PS/2 keyboard or COM1.  Then it returns, and the image's REPL takes the keyboard.

(in-package :cl-user)

(defun %t4x-key-waiting-p ()
  "A byte waiting on the 8042 (keyboard or aux) or on COM1: somebody wants the REPL."
  (or (logbitp 0 (modus-int::io-in-byte #x64))
      (logbitp 0 (modus-int::io-in-byte #x3FD))))

(defun t420-x64-main ()
  (t4-log "kiln t420 (x64): restored, ~D MHz TSC" (floor (modus-int::%pc-tsc-hz) 1000000))
  (handler-case (t4d-take)
    (error (c) (t4-log "disk: ~A" c)))
  (unless *t4-disk-own*
    (t4-log "x64: no disk of our own: no log, no command channel")
    (return-from t420-x64-main nil))
  (t4-cmd-baseline)
  (t4-log-flush)
  (loop
    (when (%t4x-key-waiting-p)
      (t4-log "x64: a key: the REPL")
      (t4-log-flush)
      (return nil))
    (handler-case (%t4-cmd-poll)
      (error (c) (t4-log "cmd poll: ~A" c)))
    (modus-int::%sleep-ms 50)))

;;; --- the desk on x64 ---------------------------------------------------------------------------
;;; Loaded after the whole desk (kiln t420 --x64 puts this file last), so these replace the i386
;;; platform calls they name.  What is not here works unchanged: the 8042, the desk's own USB
;;; drivers, the network (the same e1000 driver), the CMOS clock, the GPU's registers (PC-MMIO-MAP
;;; is the identity).  What is i386-only and stays off: the native blit routine and the
;;; write-combining code (both need i386's code area: without it the blit is the Lisp loop, which
;;; the x64 JIT makes native, and write-combining installs nothing).  The native copy and
;;; write-combining have x64 versions below, and so does the panel fitter switch.

(defun %t4-screen-up (&optional max-w max-h)
  "Take the screen UEFI left: GOP's framebuffer, as the stub recorded it (boot-uefi-x64.lisp:
   base at #x1B000, pixels a scanline at #x1B008, width and height at #x1B024 / #x1B028).
   (W H), or NIL when the firmware had no GOP."
  (declare (ignore max-w max-h))
  (when (= (mem-ref #x1B020 :u32) 1)
    (setf *t4-w* (mem-ref #x1B024 :u32) *t4-h* (mem-ref #x1B028 :u32)
          *t4-base* (mem-ref #x1B000 :u64)          ; stored pre-tagged: the value is the address
          *t4-pitch* (* 4 (mem-ref #x1B008 :u32)))
    ;; the console off the screen (2: VGA text memory, which nobody sees under GOP)
    (setf (mem-ref #x1B078 :u32) 2)
    (t4-log "x64: GOP ~Dx~D at #x~X, pitch ~D, pixel format ~D"
            *t4-w* *t4-h* *t4-base* *t4-pitch* (mem-ref #x1B00C :u32))
    (list *t4-w* *t4-h*)))

;;; THE PANEL'S OWN RESOLUTION, UNDER UEFI.  GOP on the T420 offers nothing past 1024x768 (the
;;; stub takes its largest mode; measured on the machine), the panel is 1366x768, and the panel
;;; fitter stretches the 4:3 picture -- the BIOS's picture exactly (t420.lisp's %T4-NATIVE-PANEL,
;;; whose register sequence this is).  There is no VBE EDID here, so the panel's size is pipe A's
;;; ACTIVE TIMINGS, which with the fitter scaling are the panel's own.  Read over the disk channel
;;; on the T420 with GOP at 1024x768: device #x0126, BAR0 #xF0000000, HTOTAL active 1366, VTOTAL
;;; active 768, PIPEASRC 1024x768, PF_CTL #x80800000 (on), PF_WIN_SZ 1366x768, DSPASURF 0.  The
;;; framebuffer stays at GOP's base (DSPASURF 0 is the aperture's start).
(defun %t4-native-panel ()
  "Switch an Intel GPU's pipe A from a fitted GOP mode to the panel's size.  (W H) when it did."
  (ignore-errors
   (let* ((pcr (lambda (r) (modus-int::pci-config-read 0 2 0 r)))
          (id (funcall pcr 0)))
     (when (and (= (logand id #xFFFF) #x8086) (zerop (funcall pcr #x14)))
       (let* ((bar (logand (funcall pcr #x10) (lognot 15)))
              (rd (lambda (o) (mem-ref (+ bar o) :u32)))
              (wr (lambda (o v) (setf (mem-ref (+ bar o) :u32) v)))
              (ew (1+ (logand (funcall rd #x60000) #xFFF)))
              (eh (1+ (logand (funcall rd #x6000C) #xFFF))))
         (when (and (logbitp 31 (funcall rd #x68080))            ; the fitter is scaling
                    (>= ew *t4-w*) (>= eh *t4-h*) (not (and (= ew *t4-w*) (= eh *t4-h*)))
                    (<= ew 4096) (<= eh 4096))
           (let ((pitch (* 64 (ceiling (* ew 4) 64))))
             (funcall wr #x68080 0)                                    ; PF_CTL
             (funcall wr #x68070 0)                                    ; PF_WIN_POS
             (funcall wr #x68074 0)                                    ; PF_WIN_SZ: arms the change
             (funcall wr #x6001C (logior (ash (1- ew) 16) (1- eh)))    ; PIPEASRC
             (funcall wr #x70188 pitch)                                ; DSPASTRIDE
             (funcall wr #x70180 (logior (funcall rd #x70180) #x4000)) ; DSPACNTR: trickle feed off
             (funcall wr #x7019C (funcall rd #x7019C))                 ; DSPASURF: latch
             (setf *t4-w* ew *t4-h* eh *t4-pitch* pitch
                   (mem-ref #x1B024 :u32) ew (mem-ref #x1B028 :u32) eh
                   (mem-ref #x1B008 :u32) (floor pitch 4))
             (t4-log "x64: panel ~Dx~D, fitter off, pitch ~D" ew eh pitch)
             (list ew eh))))))))

;;; No BIOS: the disk is the desk's once T4D-TAKE has run, and KILN-T420-MAIN reads the command
;;; channel's baseline before it takes the disk.  So the baseline takes it, and taking it twice
;;; is the first time's answer (a second take would reset a controller in use).
(defvar *t4x-take* (symbol-function 't4d-take))
(defun t4d-take ()
  (if (and *t4-disk-own* *t4d*) *t4d* (funcall *t4x-take*)))
(defvar *t4x-baseline* (symbol-function 't4-cmd-baseline))
(defun t4-cmd-baseline ()
  (unless *t4-disk-own* (handler-case (t4d-take) (error (c) (t4-log "disk: ~A" c))))
  (funcall *t4x-baseline*))

;;; GENERATIONAL GC: not on bare x64.  The desk's %KILN-GEN-GC-START makes a thread first, so a
;;; hosted image carves its lock arena before tenuring (app.lisp), and bare x64 has no OS
;;; threads; nor does its image carry the generational write barrier, which is built into the
;;; hosted x86-64 and AArch64 images (modus docs/generational-gc.md).  The plain collector, then,
;;; and said once.
(defun %kiln-gen-gc-start ()
  (t4-log "kiln: gc plain (no generational collector on bare x64)")
  nil)

;;; --- the native copy, x64 ------------------------------------------------------------------------
;;; t420.lisp's routine is i386 code in i386's code area; without it the copy was the Lisp loop,
;;; 1.41 s for a 1280x780 screen under QEMU (1.4 us a pixel).  This is the same routine for x86-64,
;;; in a page of the JIT's exec arena (which a saved core carries): ROWS x COLS tagged pixel words
;;; (8 bytes each, value << 1) to 32-bit pixels, its arguments in the same block at #x58000 --
;;;   +0 source address  +4 destination  +8 cols  +12 rows  +16 source stride  +20 dest pitch
;;; -- saving every register it touches (RBX RSI RDI RCX RDX R8) and answering fixnum 0 in RAX:
;;;   push rbx,rsi,rdi,rcx,rdx,r8; mov ebx,0x58000; mov esi,[rbx]; mov edi,[rbx+4]; mov r8d,[rbx+12]
;;;   row: test r8,r8; jz done; push rsi; push rdi; mov ecx,[rbx+8]
;;;   col: test ecx,ecx; jz rowend; mov rax,[rsi]; shr rax,1; mov [rdi],eax
;;;        add rsi,8; add rdi,4; dec ecx; jmp col
;;;   rowend: pop rdi; pop rsi; mov eax,[rbx+16]; add rsi,rax; mov eax,[rbx+20]; add rdi,rax
;;;        dec r8; jmp row
;;;   done: pop r8,rdx,rcx,rdi,rsi,rbx; xor eax,eax; ret
;;; Measured under QEMU: 5.9 ms for the whole screen (240x), the copied pixels checked against the
;;; vector.  A vector's payload is at its word + 7 on x64 (i386: - 5), 8 bytes a slot.
;;; *T4X-BLIT*, not t420.lisp's *T4-BLIT*: a core saved under QEMU may hold :LISP there.
(defparameter *t4x-blit-code*
  '(#x53 #x56 #x57 #x51 #x52 #x41 #x50 #xBB #x00 #x80 #x05 #x00 #x8B #x33 #x8B #x7B #x04 #x44
    #x8B #x43 #x0C #x4D #x85 #xC0 #x0F #x84 #x30 #x00 #x00 #x00 #x56 #x57 #x8B #x4B #x08 #x85
    #xC9 #x74 #x14 #x48 #x8B #x06 #x48 #xD1 #xE8 #x89 #x07 #x48 #x83 #xC6 #x08 #x48 #x83 #xC7
    #x04 #xFF #xC9 #xEB #xE8 #x5F #x5E #x8B #x43 #x10 #x48 #x01 #xC6 #x8B #x43 #x14 #x48 #x01
    #xC7 #x49 #xFF #xC8 #xEB #xC7 #x41 #x58 #x5A #x59 #x5F #x5E #x5B #x31 #xC0 #xC3))
(defvar *t4x-blit* nil)
(defun %t4-blit-fn ()
  (or *t4x-blit*
      (setf *t4x-blit*
            (let ((a (ignore-errors (modus-int::%mmap-exec-page 4096))) (i 0))
              (if (and (integerp a) (> a 4096))
                  (progn (dolist (b *t4x-blit-code*) (setf (mem-ref (+ a i) :u8) b) (setq i (+ i 1)))
                         (modus-int::%word->val (logior a 3)))
                  :lisp)))))
(defun %t4-copy (px pw sx sy cols rows dx dy)
  (let ((f (%t4-blit-fn)))
    (if (eq f :lisp)
        (dotimes (r rows)
          (let ((src (+ (* (+ sy r) pw) sx)) (dst (%t4-px-addr dx (+ dy r))))
            (dotimes (c cols)
              (setf (mem-ref dst :u32) (aref px src))
              (setq src (+ src 1) dst (+ dst 4)))))
        (progn
          (setf (mem-ref #x58000 :u32) (+ (modus-int::%val->word px) 7 (* 8 (+ (* sy pw) sx)))
                (mem-ref #x58004 :u32) (%t4-px-addr dx dy)
                (mem-ref #x58008 :u32) cols
                (mem-ref #x5800C :u32) rows
                (mem-ref #x58010 :u32) (* 8 pw)
                (mem-ref #x58014 :u32) *t4-pitch*)
          (funcall f)))
    nil))

;;; --- write-combining for the framebuffer, x64 ---------------------------------------------------
;;; t420.lisp's is i386's: PAT via i386 code, PTEs in i386's window tables at #x20000.  On x64 the
;;; framebuffer is mapped by the UEFI stub's identity map (PML4 at #x10000, 2 MB pages, flags #x83):
;;; its page-directory entries get PWT, which selects PAT entry 1, and this routine makes entry 1
;;; write-combining, then WBINVD and a CR3 reload put the entries in force:
;;;   push rax,rcx,rdx; mov ecx,0x277; rdmsr; and eax,0xFFFF00FF; or eax,0x100; wrmsr; wbinvd
;;;   mov rax,cr3; mov cr3,rax; pop rdx,rcx,rax; xor eax,eax; ret
;;; Under QEMU: 2 entries for a 1280x800 framebuffer, the desk drawing on after it (QEMU does not
;;; model caching, so the gain is the T420's: i386 measured 17.5 ms -> under 0.5 ms a drag frame).
(defparameter *t4x-wc-code*
  '(#x50 #x51 #x52 #xB9 #x77 #x02 #x00 #x00 #x0F #x32 #x25 #xFF #x00 #xFF #xFF #x0D #x00 #x01
    #x00 #x00 #x0F #x30 #x0F #x09 #x0F #x20 #xD8 #x0F #x22 #xD8 #x5A #x59 #x58 #x31 #xC0 #xC3))
(defun %t4x-install-code (bytes)
  "BYTES into a page of the JIT's exec arena, as a function; NIL where there is no arena."
  (let ((a (ignore-errors (modus-int::%mmap-exec-page 4096))) (i 0))
    (when (and (integerp a) (> a 4096))
      (dolist (b bytes) (setf (mem-ref (+ a i) :u8) b) (setq i (+ i 1)))
      (modus-int::%word->val (logior a 3)))))
(defun %t4-write-combining ()
  "Make the framebuffer write-combining: its 2 MB page-directory entries (the UEFI stub's tables,
   PML4 at #x10000) get PWT, and PAT entry 1 becomes WC.  The entries changed."
  (ignore-errors
    (let* ((lo (logand *t4-base* (lognot #x1FFFFF)))
           (hi (+ *t4-base* (* *t4-pitch* *t4-h*)))
           (pdpt (logand (mem-ref #x10000 :u32) (lognot #xFFF)))
           (n 0))
      (loop for a from lo below hi by #x200000
            do (let* ((pd (logand (mem-ref (+ pdpt (* 8 (ash a -30))) :u32) (lognot #xFFF)))
                      (pe (+ pd (* 8 (logand (ash a -21) 511))))
                      (e (mem-ref pe :u32)))
                 (when (and (logbitp 0 e) (logbitp 7 e) (= (logand e (lognot #xFFF)) (logand a #xFFE00000)))
                   (setf (mem-ref pe :u32) (logior e 8))
                   (setq n (+ n 1)))))
      (when (> n 0)
        (let ((f (%t4x-install-code *t4x-wc-code*)))
          (when f (funcall f))))
      n)))
