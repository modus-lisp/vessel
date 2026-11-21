;;;; e1000.lisp - Intel E1000 NIC driver (shared, arch-independent)
;;;;
;;;; Package MODUS.NET.E1000 (builder packages step 3).  It implements
;;;; MODUS.NET's NIC interface with E1000-HW-SEND / -RECEIVE / -RX-BUF /
;;;; -PROBE and E1000-READ-REG; an image binds the interface to them at boot
;;;; (net/nic.lisp, build-cl-repl-common's %BIND-NIC-SEAMS).
;;;;
;;;; Requires arch adapter defining:
;;;;   pci-config-read, pci-config-write, io-delay
;;;;   e1000-state-base, e1000-rx-desc-base, e1000-rx-buf-base
;;;;   e1000-tx-desc-base, e1000-tx-buf-base
;;;; (exported by MODUS-INT to the driver packages: mvm/kernel-packages.lisp)

(in-package :modus.net.e1000)

;; Two seams, identity where the CPU's addresses are the device's (x86-64,
;; QEMU virt).  The bare i386 PC overrides both (arch-i386-cl.lisp): its
;; registers sit far above what a fixnum address can name, so a BAR is mapped
;; into a window, and its RAM is paged, so a buffer's DMA address is not the
;; address the CPU writes it at.
(declaim (overridable e1000-map-bar))
(defun e1000-map-bar (phys) phys)
(declaim (overridable e1000-dma-addr))
(defun e1000-dma-addr (addr) addr)

;; The device kind at state+0x20: 0 the classic E1000 (82540EM, QEMU's e1000),
;; 1 a PCH LAN (82579LM/V, the ThinkPad T420's), whose firmware has already set
;; it up and whose management engine shares it.  state+0x24: its PCI slot.
(defun e1000-pch-p () (eql (mem-ref (+ (e1000-state-base) #x20) :u32) 1))

;; Scan PCI bus 0 for an E1000 (8086:100E) or a PCH LAN (8086:1502 / 1503).
;; Store the register base at state-base+0. Returns it or 0.  EQL, not EQ:
;; on a 30-bit tower #x100E8086 is a bignum.
(defun pci-find-e1000 ()
  (let ((found 0))
    (dotimes (dev 32)
      (let* ((id (pci-config-read 0 dev 0 0))
             (kind (cond ((eql id #x100E8086) 0)
                         ((or (eql id #x15028086) (eql id #x15038086)) 1))))
        (when (and kind (zerop found))
          ;; Enable PCI Bus Mastering + Memory Space + I/O Space
          (let ((cmd (pci-config-read 0 dev 0 4)))
            (pci-config-write 0 dev 0 4 (logior cmd 7)))
          ;; Read BAR0 (register 0x10)
          (let ((bar0 (pci-config-read 0 dev 0 #x10)))
            (setq found (e1000-map-bar (logand bar0 #xFFFFFFF0)))
            (setf (mem-ref (e1000-state-base) :u64) found)
            (setf (mem-ref (+ (e1000-state-base) #x20) :u32) kind)
            (setf (mem-ref (+ (e1000-state-base) #x24) :u32) dev)))))
    found))

;; A device register is one 32-bit access.  MEM-REF :U32 is that where a 32-bit
;; value is a fixnum; on a 30-bit tower it is two 16-bit halves (fine in RAM,
;; wrong for a register -- QEMU's e1000 answers each half with the whole
;; register), so the i386 PC overrides these with its native single-access
;; traps (arch-i386-cl.lisp).
(declaim (overridable %mmio-read32))
(defun %mmio-read32 (addr) (mem-ref addr :u32))
(declaim (overridable %mmio-write32))
(defun %mmio-write32 (addr val) (setf (mem-ref addr :u32) val))

;; Write to E1000 MMIO register
(defun e1000-write-reg (reg val)
  (%mmio-write32 (+ (mem-ref (e1000-state-base) :u64) reg) val))

;; Read E1000 MMIO register
(defun e1000-read-reg (reg)
  (%mmio-read32 (+ (mem-ref (e1000-state-base) :u64) reg)))

;; Read 16-bit word from E1000 EEPROM
(defun e1000-read-eeprom (addr)
  (let ((mmio (mem-ref (e1000-state-base) :u64)))
    ;; Write EERD: start bit | (addr << 8)
    (%mmio-write32 (+ mmio #x14) (logior 1 (ash addr 8)))
    ;; Poll for done (bit 4)
    (let ((result 0))
      (dotimes (try 10000)
        (let ((val (%mmio-read32 (+ mmio #x14))))
          (when (not (zerop (logand val 16)))
            (setq result (logand (ash val -16) #xFFFF))
            (setq try 10001))))
      result)))

;; RX ring depth, as a DEFUN so an arch adapter can override it by
;; last-defun-wins (a defvar initform would not run in-image, Limitation 7).
;;
;; WHY THIS IS TUNABLE.  The ring is the ONLY buffering between the wire and a
;; guest that can pause (a GC mid-transfer is exactly such a pause).  Measured
;; 2026-08-31 on QEMU virt: with 128 descriptors the ring holds 128*2048 =
;; 262144 bytes, and the ONLY HTTP body that ever truncated was the only one
;; larger than that -- 348160 bytes, while 194560 and 9287 always arrived whole.
;; Under ring capacity the NIC absorbs the whole response across any stall; over
;; it, a stall drops packets, TCP retransmits, and the peer closes.
;; Default stays 128 so every other arch keeps its existing DMA layout; an arch
;; that raises it MUST also move e1000-tx-desc-base/e1000-tx-buf-base clear of
;; the larger rx-buf region.
(defun e1000-rx-ring-count () 128)

;; Initialize E1000 RX descriptors
;; (e1000-rx-ring-count) descriptors, buffers at rx-buf-base (each 2048 bytes)
(defun e1000-init-rx ()
  (let ((rx-desc (e1000-rx-desc-base))
        (rx-buf (e1000-rx-buf-base)))
    (dotimes (i (e1000-rx-ring-count))
      (let ((desc-addr (+ rx-desc (* i 16)))
            (buf-addr (+ rx-buf (* i 2048))))
        ;; Buffer address low 32 bits (the device's view of it)
        (setf (mem-ref desc-addr :u32) (e1000-dma-addr buf-addr))
        ;; Buffer address high 32 bits = 0
        (setf (mem-ref (+ desc-addr 4) :u32) 0)
        ;; Clear status/length fields
        (setf (mem-ref (+ desc-addr 8) :u32) 0)
        (setf (mem-ref (+ desc-addr 12) :u32) 0)))
    ;; Set RX descriptor ring registers
    (e1000-write-reg #x2800 (e1000-dma-addr rx-desc))   ; RDBAL
    (e1000-write-reg #x2804 0)         ; RDBAH
    (e1000-write-reg #x2808 (* (e1000-rx-ring-count) 16))  ; RDLEN
    (e1000-write-reg #x2810 0)         ; RDH (head)
    (e1000-write-reg #x2818 (- (e1000-rx-ring-count) 1))    ; RDT (tail)
    ;; Store RX cursor = 0
    (setf (mem-ref (+ (e1000-state-base) #x10) :u32) 0)))

;; Initialize E1000 TX descriptors
;; 64 descriptors, buffers at tx-buf-base (each 1536 bytes)
(defun e1000-init-tx ()
  (let ((tx-desc (e1000-tx-desc-base))
        (tx-buf (e1000-tx-buf-base)))
    (dotimes (i 64)
      (let ((desc-addr (+ tx-desc (* i 16)))
            (buf-addr (+ tx-buf (* i 1536))))
        ;; Buffer address low 32 bits (the device's view of it)
        (setf (mem-ref desc-addr :u32) (e1000-dma-addr buf-addr))
        ;; Buffer address high 32 bits = 0
        (setf (mem-ref (+ desc-addr 4) :u32) 0)
        ;; Clear cmd/status fields
        (setf (mem-ref (+ desc-addr 8) :u32) 0)
        (setf (mem-ref (+ desc-addr 12) :u32) 0)))
    ;; Set TX descriptor ring registers
    (e1000-write-reg #x3800 (e1000-dma-addr tx-desc))   ; TDBAL
    (e1000-write-reg #x3804 0)         ; TDBAH
    (e1000-write-reg #x3808 1024)      ; TDLEN (64 * 16)
    (e1000-write-reg #x3810 0)         ; TDH (head)
    (e1000-write-reg #x3818 0)         ; TDT (tail)
    ;; Store TX cursor = 0
    (setf (mem-ref (+ (e1000-state-base) #x14) :u32) 0)))

;; Full E1000 initialization
(defun e1000-init ()
  (if (e1000-pch-p) (e1000-init-pch) (e1000-init-classic)))

(defun e1000-init-classic ()
  (let ((mmio (mem-ref (e1000-state-base) :u64))
        (state (e1000-state-base)))
    (when (zerop mmio)
      ;; "E1000:No" + newline
      (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
      (%serial-byte 48) (%serial-byte 58) (%serial-byte 78) (%serial-byte 111)
      (%serial-byte 10)
      (return 0))

    ;; 1. Reset: write CTRL.RST (bit 26)
    (e1000-write-reg 0 #x04000000)
    ;; Wait for reset to clear
    (dotimes (i 10000)
      (when (zerop (logand (e1000-read-reg 0) #x04000000))
        (setq i 10001)))

    ;; 2. Read MAC from EEPROM
    (let ((mac0 (e1000-read-eeprom 0))
          (mac1 (e1000-read-eeprom 1))
          (mac2 (e1000-read-eeprom 2)))
      ;; Store MAC bytes at state+0x08
      (setf (mem-ref (+ state #x08) :u8) (logand mac0 #xFF))
      (setf (mem-ref (+ state #x09) :u8) (logand (ash mac0 -8) #xFF))
      (setf (mem-ref (+ state #x0A) :u8) (logand mac1 #xFF))
      (setf (mem-ref (+ state #x0B) :u8) (logand (ash mac1 -8) #xFF))
      (setf (mem-ref (+ state #x0C) :u8) (logand mac2 #xFF))
      (setf (mem-ref (+ state #x0D) :u8) (logand (ash mac2 -8) #xFF))

      ;; 3. Set MAC in RAL0/RAH0
      (e1000-write-reg #x5400 (logior mac0 (ash mac1 16)))
      (e1000-write-reg #x5404 (logior mac2 #x80000000))  ; AV bit

      ;; Print "MAC:" then hex bytes
      (%serial-byte 77) (%serial-byte 65) (%serial-byte 67) (%serial-byte 58)
      (print-hex-byte (logand mac0 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac0 -8) #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand mac1 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac1 -8) #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand mac2 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac2 -8) #xFF))
      (%serial-byte 10))

    ;; 4. Clear multicast table (128 dwords at 0x5200)
    (dotimes (i 128)
      (e1000-write-reg (+ #x5200 (* i 4)) 0))

    ;; 5. Setup RX ring
    (e1000-init-rx)

    ;; 6. Setup TX ring
    (e1000-init-tx)

    ;; 7. Init link: CTRL = SLU (bit 6) | ASDE (bit 5)
    (e1000-write-reg 0 (logior #x40 #x20))

    ;; 8. Enable RX: RCTL = EN | BAM | BSIZE_2048 | SECRC
    (e1000-write-reg #x100 (logior 2 (logior #x8000 #x04000000)))

    ;; 9. Enable TX: TCTL = EN | PSP | CT=0x0F | COLD=0x40
    (e1000-write-reg #x400 (logior 2 (logior 8 (logior #xF0 #x40000))))

    ;; 10. Set TIPG
    (e1000-write-reg #x410 (logior 10 (logior (ash 8 10) (ash 6 20))))

    ;; 11. TXDCTL: Queue Enable (bit 25)
    (e1000-write-reg #x3828 (ash 1 25))

    ;; 12. Clear TX delay registers
    (e1000-write-reg #x3820 0)   ; TIDV
    (e1000-write-reg #x382C 0)   ; TADV

    ;; Enable RXT0 interrupt (bit 7) — RX timer fires on packet receive.
    ;; IRQs stay masked at CPU (PSTATE.I=1 / EFLAGS.IF=0) so no ISR runs,
    ;; but the pending interrupt signal wakes HLT/WFI in io-delay.
    ;; Clear all first, then set RXT0.
    (e1000-write-reg #xD8 #xFFFFFFFF)  ; IMC: clear all
    (e1000-write-reg #xD0 #x80)        ; IMS: enable RXT0 (bit 7)

    ;; Start UNCONFIGURED (0.0.0.0) — DHCP must supply the address.
    ;;
    ;; This used to preload 10.0.2.15 / 10.0.2.2 with the comment "will be
    ;; overwritten by DHCP".  Those are exactly slirp's first lease and its
    ;; gateway, so when DHCP FAILED the stale defaults still routed under QEMU
    ;; and the run looked like a success — the one failure the network bring-up
    ;; most needs to report was the one it could not show.  A value that
    ;; happens to match the expected answer is not evidence the mechanism ran.
    ;;
    ;; 0.0.0.0 is also the correct source address for a DHCP DISCOVER, so this
    ;; is what an unconfigured interface should hold anyway.  Anything needing
    ;; a static address must now set it explicitly and visibly.
    (setf (mem-ref (+ state #x18) :u32) 0)
    (setf (mem-ref (+ state #x1C) :u32) 0)

    ;; "E1000:OK" + newline
    (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
    (%serial-byte 48) (%serial-byte 58) (%serial-byte 79) (%serial-byte 75)
    (%serial-byte 10)
    1))

;;; ---------------------------------------------------------------------------
;;; PCH LAN (82579LM / 82579V): the ThinkPad T420's.  BIOS-PRESERVING: the
;;; firmware has configured the MAC and the PHY, and the management engine
;;; (ME/AMT) shares the device, so there is no CTRL.RST (a reset loses the
;;; firmware's setup -- how this NIC was first brought up on a T420, in the
;;; mini-Lisp image, 2026-04).  Linux e1000e's order otherwise: tell the ME a
;;; driver is loaded, the ICH/PCH bits, the rings, link by read-modify-write
;;; (never a blind CTRL write), RX, then TX with its PCH thresholds, TCTL last.

(defun e1000-me-wait ()
  "Until the management engine releases the bus (FWSM bit 24): a register
   write made while it is accessing the device is dropped."
  (dotimes (i 2000)
    (when (zerop (logand (e1000-read-reg #x5B54) #x1000000)) (return t))
    (io-delay)))

(defun e1000-rmw (reg set)
  (e1000-me-wait)
  (e1000-write-reg reg (logior (e1000-read-reg reg) set)))

(defun e1000-init-pch ()
  (let ((state (e1000-state-base)))
    ;; the ME: a host driver is loaded (SWSM.DRV_LOAD, CTRL_EXT.DRV_LOAD)
    (e1000-rmw #x5B50 1)
    ;; CTRL_EXT: bit 22 (required on ICH/PCH), 28 DRV_LOAD, 17 RO_DIS
    (e1000-rmw #x18 #x10420000)
    ;; GCR: no NO_SNOOP bits, so the NIC's DMA snoops the CPU's caches
    (e1000-me-wait)
    (e1000-write-reg #x5B00 (logand (e1000-read-reg #x5B00) (lognot #x3F)))
    ;; errata: RFCTL descriptor corruption bits, KABGTXD TX arbitration
    (e1000-rmw #x5008 #x3C0)
    (e1000-rmw #x3004 #x50000)
    ;; the MAC the firmware left in RAL0/RAH0
    (let ((ral (e1000-read-reg #x5400)) (rah (e1000-read-reg #x5404)))
      (dotimes (i 4) (setf (mem-ref (+ state #x08 i) :u8) (logand (ash ral (* -8 i)) #xFF)))
      (setf (mem-ref (+ state #x0C) :u8) (logand rah #xFF))
      (setf (mem-ref (+ state #x0D) :u8) (logand (ash rah -8) #xFF))
      (%serial-byte 77) (%serial-byte 65) (%serial-byte 67) (%serial-byte 58)
      (dotimes (i 6)
        (when (> i 0) (%serial-byte 58))
        (print-hex-byte (mem-ref (+ state #x08 i) :u8)))
      (%serial-byte 10))
    (dotimes (i 128) (e1000-write-reg (+ #x5200 (* i 4)) 0))
    (e1000-init-rx)
    (e1000-init-tx)
    ;; link: set SLU, keep everything the firmware chose
    (e1000-rmw 0 #x40)
    ;; RX: EN | BAM | BSIZE 2048 | SECRC
    (e1000-me-wait)
    (e1000-write-reg #x100 #x4008002)
    ;; TX, Linux's order: TIPG, TXDCTL (PTHRESH 31, WTHRESH 1, no GRAN), TARC0, delays, TCTL last
    (e1000-me-wait)
    (e1000-write-reg #x410 #x602008)
    (e1000-me-wait)
    (e1000-write-reg #x3828 #x0101001F)
    (e1000-rmw #x3840 #xD800081)
    (e1000-write-reg #x3820 8)
    (e1000-write-reg #x382C 32)
    ;; TCTL: EN | PSP | CT 15 | COLD 63 | RTLC | MULR (bit 28: a second DMA
    ;; request, without which a descriptor's data is never fetched) | bit 29
    (e1000-me-wait)
    (e1000-write-reg #x400 #x3103F0FA)
    ;; STATUS.PHYRA, and interrupts as the classic path has them
    (e1000-write-reg 8 #x400)
    (e1000-write-reg #xD8 #xFFFFFFFF)
    (e1000-read-reg #xC0)
    (e1000-write-reg #xD0 #x80)
    ;; with AMT running, let the ME pass the host its traffic (MANC.EN_MNG2HOST)
    (when (logtest (e1000-read-reg #x5B54) #x8000)
      (e1000-rmw #x5820 #x1000))
    (setf (mem-ref (+ state #x18) :u32) 0)
    (setf (mem-ref (+ state #x1C) :u32) 0)
    ;; "E1000:PCH" + newline
    (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
    (%serial-byte 48) (%serial-byte 58) (%serial-byte 80) (%serial-byte 67)
    (%serial-byte 72) (%serial-byte 10)
    1))

(defun e1000-status ()
  "What the NIC says, for a person at the REPL: (STATUS CTRL RDH RDT TDH TDT
   FWSM), raw.  STATUS bit 1 is link up."
  (mapcar (function e1000-read-reg) (list 8 0 #x2810 #x2818 #x3810 #x3818 #x5B54)))

;; Send raw ethernet frame from byte array
(defun e1000-hw-send (buf len)
  (let ((state (e1000-state-base))
        (tx-desc (e1000-tx-desc-base))
        (tx-buf (e1000-tx-buf-base)))
    (let ((tx-cur (mem-ref (+ state #x14) :u32)))
      (let ((desc-addr (+ tx-desc (* tx-cur 16)))
            (buf-addr (+ tx-buf (* tx-cur 1536))))
        ;; Copy data to TX buffer
        (dotimes (i len)
          (setf (mem-ref (+ buf-addr i) :u8) (aref buf i)))
        ;; Set descriptor: length in low 16 bits of +8, cmd in byte at +11
        ;; cmd = EOP(1) | IFCS(2) | RS(8) = 0x0B
        (setf (mem-ref (+ desc-addr 8) :u32)
              (logior len (ash #x0B 24)))
        ;; Clear status
        (setf (mem-ref (+ desc-addr 12) :u32) 0)
        ;; Bump TX tail
        ;; Ring wrap WITHOUT (mod …): compile-mod expands to
        ;; (nth-value 1 (truncate …)) — a multiple-value return plus a
        ;; runtime NTH, machinery a bare net image cannot rely on.  It
        ;; produced 0 instead of 1 on aarch64, so TDT never advanced past
        ;; TDH and NOTHING was ever transmitted.  A compare-and-reset is
        ;; both correct and cheaper.  Task #219.
        (let ((next (if (< (+ tx-cur 1) 64) (+ tx-cur 1) 0)))
          (setf (mem-ref (+ state #x14) :u32) next)
          (e1000-write-reg #x3818 next)
          ;; Wait for TX done: poll TDH register
          (let ((done 0))
            (dotimes (try 100000)
              (when (eq (e1000-read-reg #x3810) next)
                (setq done 1)
                (setq try 100001)))
            done))))))

;; Check for received packet. Returns length or 0 if none.
;; Reads ICR to clear pending interrupt cause (allows HLT/WFI to sleep
;; until next packet arrives, rather than waking immediately on stale cause).
(defun e1000-hw-receive ()
  (let ((state (e1000-state-base)))
    (let ((rx-cur (mem-ref (+ state #x10) :u32)))
      ;; Check if E1000 has advanced RDH past our cursor
      (if (eq (e1000-read-reg #x2810) rx-cur)
          (progn
            ;; No packet — read ICR to clear pending interrupt cause
            (e1000-read-reg #xC0)
            0)
          ;; Packet received! Get length from descriptor
          (let ((desc-addr (+ (e1000-rx-desc-base) (* rx-cur 16))))
            (let ((pkt-len (mem-ref (+ desc-addr 8) :u16)))
              ;; Advance cursor, then DEFERRED RE-ARM.
              ;;
              ;; RDT must NOT be set to rx-cur here.  Writing rx-cur hands that
              ;; descriptor straight back to the NIC while its buffer is still
              ;; unread: the caller only fetches it AFTER this returns, via
              ;; e1000-hw-rx-buf (which deliberately returns rx-cur-1, "the
              ;; previous cursor's buffer, since we already advanced") and then
              ;; copies it.  So the NIC owned a buffer the driver was still
              ;; reading, and at line rate it overwrote it.
              ;;
              ;; Interpreted, packets arrived sparsely enough relative to
              ;; processing that the window almost never closed on us.  Once the
              ;; aarch64 JIT made this loop native the driver re-armed instantly,
              ;; the NIC always had free descriptors and streamed flat out, and a
              ;; 348160-byte HTTP body came back TRUNCATED at 97723 / 182683 /
              ;; 226080 / 298080 bytes -- varying, because it is a race.  TCP saw
              ;; the loss, retransmitted, and the peer eventually closed
              ;; (RXEND why=1), which is what proved this was data loss and not
              ;; the receive loop's own timeout.
              ;;
              ;; Re-arm rx-cur-1 instead: the descriptor consumed by the PREVIOUS
              ;; call, which the caller has provably finished with.  Costs one
              ;; descriptor of ring depth (127 of 128 usable) and needs no extra
              ;; state.  On the very first call prev = 127, which is exactly the
              ;; RDT the init path already programmed, so it is a no-op.
              ;;
              ;; Same defect and same fix as the Pi's r8152 RX DMA race.  A
              ;; latent race is a function of RELATIVE speed, so a speedup is a
              ;; correctness change: after one, re-audit everything that hands a
              ;; buffer to hardware.
              (let ((next (if (< (+ rx-cur 1) (e1000-rx-ring-count))
                              (+ rx-cur 1) 0))
                    (prev (if (zerop rx-cur)
                              (- (e1000-rx-ring-count) 1)
                              (- rx-cur 1))))
                (setf (mem-ref (+ state #x10) :u32) next)
                (e1000-write-reg #x2818 prev))
              pkt-len))))))

;; Get pointer to current RX buffer data
(defun e1000-hw-rx-buf ()
  (let ((rx-cur (mem-ref (+ (e1000-state-base) #x10) :u32)))
    ;; Return previous cursor's buffer since we already advanced
    (let ((prev (if (zerop rx-cur) (- (e1000-rx-ring-count) 1) (- rx-cur 1))))
      (+ (e1000-rx-buf-base) (* prev 2048)))))

;; Find E1000 and initialize. Main entry point.
(defun e1000-hw-probe ()
  (let ((mmio (pci-find-e1000)))
    (if (zerop mmio)
        (progn
          ;; "E1000:NF" + newline
          (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
          (%serial-byte 48) (%serial-byte 58) (%serial-byte 78) (%serial-byte 70)
          (%serial-byte 10)
          0)
        (progn
          ;; "E1000:MMIO=" then hex32
          (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
          (%serial-byte 48) (%serial-byte 58)
          (print-hex32 (logand mmio #xFFFFFFFF))
          (%serial-byte 10)
          (e1000-init)))))
