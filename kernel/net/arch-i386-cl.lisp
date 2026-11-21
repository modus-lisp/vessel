;;;; arch-i386-cl.lisp — QEMU pc (i386) NE2000 adapter for the BARE-METAL
;;;; CL/mvm image built by mvm/build-i386-cl-repl.lisp (platform :i386 of
;;;; mvm/build-cl-repl-common.lisp).
;;;;
;;;; The i386 sibling of net/arch-x86-cl.lisp — same contract (the names
;;;; net/ip.lisp and http-client.lisp call), on the NE2000 ISA NIC that
;;;; net/ne2000.lisp drives.  It is NOT net/arch-i386.lisp: that file serves the
;;;; `repl-source' mini-Lisp images (a PIT-timer io-delay, an SSH capture
;;;; %serial-byte at #x540000, line-editor slots), all at addresses inside this
;;;; ~42 MB image.  Nor does this image take net/32bit-overrides.lisp, which
;;;; truncates 32-bit quantities to 30 bits: the CL runtime has bignums, and
;;;; MEM-REF :U32 reads and writes them whole on a 32-bit target.
;;;;
;;;; SPLICED AFTER net/ne2000.lisp, not before it as the other adapters are:
;;;; the driver defines its host buffers NE2K-RX-HOST / NE2K-TX-HOST at
;;;; #x780000 / #x781000 — inside this image — and under last-defun-wins only a
;;;; later definition moves them.
;;;;
;;;; Memory: the net block at #x04000000 .. #x04400000, which the boot stub
;;;; (boot/boot-i386-cl.lisp) maps from RAM on a machine of 32 MB or more and
;;;; whose physical address it records at #x80AC -- the E1000's rings are DMA'd.
;;;; NE2000 is port I/O.

;; ============================================================
;; PCI configuration space: QEMU's NE2000 is ISA, but a PC's own NIC (an E1000
;; or a T420's 82579LM, e1000.lisp) is on PCI.  The address written to 0xCF8
;; needs bit 31 and the value read from 0xCFC may need 32 -- neither is a
;; fixnum on this tower -- so the cycle is translate-i386's native trap #x0333
;; (PCI-CONFIG-READ-RAW), which leaves the raw value in its scratch
;; (*I386-RAW-SCRATCH* + 8 = #x1B0A8, a bare CL image's), read back whole.
;; ============================================================
(in-package :modus.board)

(defun %pci-addr (bus dev fn reg)
  (logior (ash bus 16) (logior (ash dev 11) (logior (ash fn 8) (logand reg #xFC)))))

(defun pci-config-read (bus dev fn reg)
  (pci-config-read-raw (%pci-addr bus dev fn reg))
  (mem-ref #x1B0A8 :u32))

(defun pci-config-write (bus dev fn reg val)
  ;; the read leaves the address latched in 0xCF8; then the data port.  VAL a
  ;; fixnum: the command register and the like, which is all a driver writes
  (pci-config-read-raw (%pci-addr bus dev fn reg))
  (io-out-dword #xCFC val))

(defun pci-assign-bars () nil)

;; ============================================================
;; Device registers and DMA (boot/boot-i386-cl.lisp's MMIO window and net block)
;; ============================================================
;; MMIO: a BAR sits far above what a fixnum address can name, so its pages go
;; into the 4 MB window at #x0D000000, whose page table is at PA = VA #x2A000,
;; uncached (PCD|PWT).  x86 caches no not-present entry: no flush.
(defvar *pc-mmio-next* 0)

(defun pc-mmio-map (phys bytes)
  "BYTES of device registers at physical PHYS: their address in the window."
  (let* ((base (- phys (logand phys #xFFF))) (off (logand phys #xFFF))
         (n (ceiling (+ off bytes) 4096)) (next *pc-mmio-next*) (i 0))
    (when (> (+ next n) 1024) (error "pc-mmio-map: the window is full"))
    (loop (when (>= i n) (return nil))
      (setf (mem-ref (+ #x2A000 (* 4 (+ next i))) :u32) (logior (+ base (* i 4096)) #x1B))
      (setq i (+ i 1)))
    (setq *pc-mmio-next* (+ next n))
    (+ #x0D000000 (* next 4096) off)))

;; A register access must be ONE 32-bit load or store; MEM-REF :U32 on this
;; tower is two 16-bit halves.  translate-i386's traps #x0330 / #x0331 make the
;; single access, the address and value passing through the raw scratch
;; (#x1B0A0 / #x1B0A8: RAM, where halves are fine).
(defun %mmio-read32 (addr)
  (setf (mem-ref #x1B0A0 :u32) addr)
  (mmio-do-read32)
  (mem-ref #x1B0A8 :u32))

(defun %mmio-write32 (addr val)
  (setf (mem-ref #x1B0A0 :u32) addr)
  (setf (mem-ref #x1B0A8 :u32) val)
  (mmio-do-write32)
  val)

;; DMA: the net block, VA #x04000000 .. #x04400000, is RAM the stub mapped on a
;; machine of 32 MB or more; its physical address is the stub's word at #x80AC.
(defun pc-net-block-p () (not (zerop (mem-ref #x80AC :u32))))

(defun e1000-dma-addr (addr)
  "e1000.lisp's seam: the device's address for net-block address ADDR."
  (+ (- addr #x04000000) (mem-ref #x80AC :u32)))

(defun e1000-map-bar (phys)
  "e1000.lisp's seam: the E1000's registers (128 KB) in the MMIO window."
  (pc-mmio-map phys #x20000))

;; the E1000's rings and buffers, in the net block past the state below
(defun e1000-rx-desc-base () #x04200000)   ; 128 x 16
(defun e1000-tx-desc-base () #x04201000)   ; 64 x 16
(defun e1000-rx-buf-base ()  #x04210000)   ; 128 x 2048
(defun e1000-tx-buf-base ()  #x04250000)   ; 64 x 1536

;; ============================================================
;; Timing / serial / entropy
;; ============================================================
;; About 100 us of REAL time.  It was 5000 RAM reads, which is a duration only
;; while the code is slow: with the native tier those loops run in
;; microseconds, and ARP-RESOLVE's 200 rounds / TCP-CONNECT's 500 were spent
;; before QEMU had delivered the reply -- ARP:0, TCP:F, a fetch that never sent
;; its SYN.  Waiting for the PIT's next tick (10 ms) is too coarse: DHCP's and
;; TCP's loops are sized for a short delay.  So the spin is CALIBRATED: the
;; first call counts how many iterations fit in one tick (the timer interrupt
;; counts ticks at #x8100, 100 Hz) and every call spins a hundredth of that.
;; RAM reads, not port I/O (see arch-x86-cl.lisp: port-I/O spins hold QEMU's
;; big lock and starve the iothread that queues slirp's replies).  The count
;; is per boot -- a saved core's would be the core-building machine's -- in the
;; net block, which the stub zeroes at every boot (a defvar when there is none).
(defvar *io-delay-iters* 0)

(defun %io-delay-calibrate ()
  "Spin iterations per 100 us, by the PIT tick; 5000 if the timer is not running."
  (let ((t0 (mem-ref #x8100 :u32)) (i 0))
    (loop (when (or (>= i 50000000) (not (eql (mem-ref #x8100 :u32) t0))) (return nil))
      (setq i (+ i 1)))
    (if (>= i 50000000)
        5000
        (let ((t1 (mem-ref #x8100 :u32)) (n 0))
          (loop (when (or (>= n 200000000) (not (eql (mem-ref #x8100 :u32) t1))) (return nil))
            (setq n (+ n 1)))
          (max 20 (floor n 100))))))

(defun %io-delay-iters ()
  (let ((cell (if (zerop (mem-ref #x80AC :u32)) nil #x043FFFF0)))
    (let ((k (if cell (mem-ref cell :u32) *io-delay-iters*)))
      (if (and k (> k 0))
          k
          (let ((c (%io-delay-calibrate)))
            (if cell (setf (mem-ref cell :u32) c) (setq *io-delay-iters* c))
            c)))))

(defun io-delay ()
  (let ((k (%io-delay-iters)) (i 0) (t0 (mem-ref #x8100 :u32)))
    (loop (when (>= i k) (return nil))
      (when (eql (mem-ref #x8100 :u32) -1) (return nil))   ; the same read as calibration's
      (setq i (+ i 1)))
    t0
    nil))

(defun %serial-byte (b)
  (write-char-serial b))

(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (io-in-byte #x80)
      (setq s (logxor (ash s 8) (logand (io-in-byte #x40) #xFF))))
    (when (zerop s) (setq s 42))
    s))

(defun print-hex-digit (n)
  (if (< n 10)
      (%serial-byte (+ n 48))
      (%serial-byte (+ n 55))))

(defun print-hex-byte (b)
  (let ((hi (logand (ash b -4) 15))
        (lo (logand b 15)))
    (print-hex-digit hi)
    (print-hex-digit lo)))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

;; ============================================================
;; NE2000 host buffers and driver state
;; ============================================================
(defun ne2k-rx-host ()       #x04000000)
(defun ne2k-tx-host ()       #x04001000)
(declaim (overridable e1000-state-base))
(defun e1000-state-base ()   #x04060000)
(declaim (overridable fe-scratch-base))
(defun fe-scratch-base ()    #x04060900)
(declaim (overridable ssh-conn-base))
(defun ssh-conn-base ()      #x04080000)
(declaim (overridable ssh-ipc-base))
(defun ssh-ipc-base ()       #x04100000)
