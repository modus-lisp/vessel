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
