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
;;; the x64 JIT makes native, and write-combining installs nothing), and the panel fitter switch
;;; (VBE's EDID; UEFI's GOP mode is the panel's own already).

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
