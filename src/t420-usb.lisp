;;;; t420-usb.lisp -- USB keyboards and mice on a PC's EHCI controller (the T420's).
;;;;
;;;; THE T420'S BUS (read from Linux on this machine, 2026-11-23).  Two EHCI controllers, each
;;;; with Intel's rate-matching hub (8087:0024) on root port 1 and no companion controllers:
;;;;   00:1a.0  RMH: port 1 the boot disk (the rig's Zero), 5 the dock's hub, 6 the camera
;;;;   00:1d.0  RMH: port 2 an Anker USB-C hub (high speed, multi-TT): a Keychron Q3 (full
;;;;            speed, boot keyboard), a Microsoft mouse (low speed, boot mouse), an RTL8153,
;;;;            a card reader
;;;; The BIOS boots from the first and goes on reading the disk through it (INT 13h: the desk's
;;;; log and command channel), so only controllers in *T4-USB-CONTROLLERS* are taken -- by
;;;; default the second.  Taking one ends the BIOS's use of it (the USBLEGSUP handoff).
;;;;
;;;; THE SAME DESIGN AS THE ZERO'S (kernel/net/usb-tree.lisp, usb-hid-split.lisp): the tree is
;;;; walked from the root port, hubs inside hubs, each port reset alone and its device
;;;; addressed; a full- or low-speed device is reached through the transaction translator of
;;;; the nearest high-speed hub above it; the first boot keyboard and boot mouse are bound and
;;;; their boot reports decoded.  What differs is the controller: EHCI does the split
;;;; transactions itself, given the hub and port in each endpoint's queue head, and polls
;;;; interrupt endpoints from its periodic schedule.  This is runtime code (the desk system), so
;;;; it can be redefined from the disk command channel while the desk runs; once it has run on
;;;; the hardware it is meant to become usb-tree's EHCI backend in the kernel.
;;;;
;;;; MEMORY.  The controller reads its schedules by physical address, so they live in the net
;;;; block (#x0F000000, mapped by the boot stub, physical base at #x80AC), in the quarter the
;;;; E1000 leaves alone: #x0F300000 .. #x0F30FFFF.  Every word the controller may read while it
;;;; runs is written with ONE 32-bit store (%MMIO-WRITE32): a MEM-REF :U32 store on this tower is
;;;; two 16-bit halves, and a token half-written is a transfer half-armed.

;;; ---- configuration ----------------------------------------------------------------------------

(defparameter *t4-usb-controllers* '((0 #x1D 0))
  "PCI (bus dev fn) of the EHCI controllers to take.  Not the BIOS's boot disk's.")

(defvar *t4-usb* nil "The controller in use: a plist (:bdf :ops :ports), or NIL.")
(defvar *t4-usb-devices* nil
  "The bus, as enumerated: plists (:addr :parent :port :speed :tthub :ttport :id :class :name
   :mps0 :hub-ports).  Speed 0 full, 1 low, 2 high (EHCI's EPS encoding).")
(defvar *t4-usb-log* nil "What bring-up said, newest first.")

(defun %t4u-say (fmt &rest args)
  (let ((s (apply #'format nil fmt args)))
    (push s *t4-usb-log*)
    (when (fboundp 't4-log) (funcall 't4-log "usb: ~A" s))
    s))

;;; ---- memory ---------------------------------------------------------------------------------

(defconstant +t4u-area+ #x0F300000)
(defvar *t4u-area* +t4u-area+
  "The schedules' area of the controller being worked: the keyboard's and mouse's controller
   here, the boot disk's elsewhere (t420-usb-disk.lisp binds it, with *T4-USB* and the bus).")
(defun %t4u-va (off) (+ *t4u-area* off))
(defun %t4u-pa (va) (+ (- va #x0F000000) (mem-ref #x80AC :u32)))
(defun %t4u-w (va v) (modus-int::%mmio-write32 va (logand v #xFFFFFFFF)))
(defun %t4u-r (va) (modus-int::%mmio-read32 va))

;; A QH is 68 bytes and a qTD 52 on a controller with 64-bit addressing (HCCPARAMS bit 0, as
;; the PCH's are): five more dwords, the upper halves of the buffer pointers, which the
;; controller writes back with the overlay.  At 64 bytes apart the keyboard QH's write-back
;; zeroed the mouse QH's link (its dword 0, at +64): a link of 0 is an iTD at PHYSICAL 0, and
;; the controller walked the real-mode interrupt table every frame -- the hangs.  128 apart.
(defun %t4u-frame-list () (%t4u-va #x0000))       ; 1024 x 4, 4 KB aligned
(defun %t4u-head-qh () (%t4u-va #x1000))           ; the async ring's head
(defun %t4u-ctl-qh () (%t4u-va #x1080))
(defun %t4u-ctl-td (k) (%t4u-va (+ #x1100 (* k 64))))   ; setup, data, status
(defun %t4u-int-qh (k) (%t4u-va (+ #x1200 (* k 128))))  ; 0 keyboard, 1 mouse
(defun %t4u-int-td (k) (%t4u-va (+ #x1300 (* k 64))))
(defun %t4u-setup-buf () (%t4u-va #x1400))
(defun %t4u-data-buf () (%t4u-va #x2000))           ; one page: control data
(defun %t4u-int-buf (k) (%t4u-va (+ #x3000 (* k 64))))

(defun %t4u-clear (va bytes) (loop for o from 0 below bytes by 4 do (%t4u-w (+ va o) 0)))
(defun %t4u-byte (va i) (mem-ref (+ va i) :u8))
(defun %t4u-u16 (va i) (logior (%t4u-byte va i) (ash (%t4u-byte va (+ i 1)) 8)))

;;; ---- the controller's registers ---------------------------------------------------------------

(defun %t4u-op (off) (+ (getf *t4-usb* :ops) off))
(defun %t4u-opr (off) (%t4u-r (%t4u-op off)))
(defun %t4u-opw (off v) (%t4u-w (%t4u-op off) v))
(defun %t4u-portsc (p) (%t4u-op (+ #x44 (* 4 (- p 1)))))
(defun %t4u-sleep (ms) (modus-int::%sleep-ms ms))

(defun %t4u-wait (pred ms)
  "Poll PRED until it holds, for up to MS by the clock: T when it held.  (It slept a
   \"millisecond\" between polls, and the bare %SLEEP-MS is a calibrated spin that is far longer
   on the T420: a 32 KB disk read took 30 ms, 1.0 MB/s; polling, 38.5 MB/s.)"
  (let ((end (+ (get-internal-real-time) (* ms (floor internal-time-units-per-second 1000)))))
    (loop (when (funcall pred) (return t))
          (when (> (get-internal-real-time) end) (return nil)))))

(defun %t4u-take (bus dev fn)
  "Take the EHCI controller at BUS:DEV.FN from the BIOS, reset it and start its schedules.
   The plist for *T4-USB*, or NIL."
  (let* ((id (modus-int::pci-config-read bus dev fn 0))
         (class (ash (modus-int::pci-config-read bus dev fn 8) -8)))
    (unless (= class #x0C0320)
      (%t4u-say "~X:~X.~X is not EHCI (class ~6,'0X)" bus dev fn class)
      (return-from %t4u-take nil))
    ;; memory space and bus mastering on
    (modus-int::pci-config-write bus dev fn 4 (logior (modus-int::pci-config-read bus dev fn 4) 6))
    (let* ((bar (logand (modus-int::pci-config-read bus dev fn #x10) (lognot #xFF)))
           (base (modus-int::pc-mmio-map bar #x400))
           (caplen (logand (%t4u-r base) #xFF))
           (hcs (%t4u-r (+ base 4)))
           (hcc (%t4u-r (+ base 8)))
           (eecp (logand (ash hcc -8) #xFF))
           (*t4-usb* (list :bdf (list bus dev fn) :ops (+ base caplen) :ports (logand hcs #xF)
                           :id id)))
      ;; THE BIOS HANDOFF: USBLEGSUP (capability 1) -- set OS-owned, wait for BIOS-owned to clear,
      ;; and turn off the SMIs it routed to the BIOS
      (when (>= eecp #x40)
        (let ((leg (modus-int::pci-config-read bus dev fn eecp)))
          (when (= (logand leg #xFF) 1)
            ;; as Linux's ehci_bios_handoff: SMI on OS-ownership change on first, so the BIOS's
            ;; handler hears of the request (without it a BIOS that is busy with the controller --
            ;; the boot disk's, at the desk's start -- is not told, and keeps driving it)
            (when (logbitp 16 leg)
              (modus-int::pci-config-write bus dev fn (+ eecp 4)
                                           (logior (modus-int::pci-config-read bus dev fn (+ eecp 4)) (ash 1 13))))
            (modus-int::pci-config-write bus dev fn eecp (logand (logior leg (ash 1 24)) (lognot (ash 1 16))))
            (unless (%t4u-wait (lambda () (not (logbitp 16 (modus-int::pci-config-read bus dev fn eecp)))) 5000)
              (%t4u-say "the BIOS kept ~X:~X.~X; taking it anyway" bus dev fn))
            ;; every SMI off (and their status bits cleared)
            (modus-int::pci-config-write bus dev fn (+ eecp 4) #xE0000000))))
      ;; stop, reset
      (%t4u-opw 0 (logand (%t4u-opr 0) (lognot 1)))
      (%t4u-wait (lambda () (logbitp 12 (%t4u-opr 4))) 50)
      (%t4u-opw 0 2)
      (unless (%t4u-wait (lambda () (not (logbitp 1 (%t4u-opr 0)))) 250)
        (%t4u-say "reset did not finish")
        (return-from %t4u-take nil))
      ;; the schedules: an async ring of one head QH, and a frame list that points nowhere yet
      (%t4u-clear *t4u-area* #x4000)
      (dotimes (i 1024) (%t4u-w (+ (%t4u-frame-list) (* 4 i)) 1))
      (let ((h (%t4u-head-qh)))
        (%t4u-w h (logior (%t4u-pa h) 2))
        (%t4u-w (+ h 4) (logior (ash 1 15) (ash 2 12) (ash 64 16)))   ; H, high speed
        (%t4u-w (+ h 8) (ash 1 30))
        (%t4u-w (+ h 16) 1) (%t4u-w (+ h 20) 1))
      (%t4u-opw #x10 0)                                ; CTRLDSSEGMENT
      (%t4u-opw 8 0)                                   ; no interrupts: polled
      (%t4u-opw #x14 (%t4u-pa (%t4u-frame-list)))
      (%t4u-opw #x18 (%t4u-pa (%t4u-head-qh)))
      ;; run: 1024-entry list, periodic + async on, an interrupt threshold of 1 microframe
      (%t4u-opw 0 (logior (ash 1 16) (ash 1 5) (ash 1 4) 1))
      (%t4u-opw #x40 1)                                ; CONFIGFLAG: every port routed here
      (%t4u-wait (lambda () (not (logbitp 12 (%t4u-opr 4)))) 50)
      (dotimes (p (getf *t4-usb* :ports))              ; port power, where the ports switch it
        (let ((sc (%t4u-portsc (+ p 1))))
          (%t4u-w sc (logior (logand (%t4u-r sc) (lognot #x2A)) (ash 1 12)))))
      (%t4u-sleep 50)
      (%t4u-say "took ~X:~X.~X (~8,'0X), ~D ports" bus dev fn id (getf *t4-usb* :ports))
      *t4-usb*)))

;;; ---- control transfers ----------------------------------------------------------------------

(defun %t4u-td (va next toggle bytes pid buf)
  (%t4u-w va (if next (%t4u-pa next) 1))
  (%t4u-w (+ va 4) 1)
  (%t4u-w (+ va 12) (if buf (%t4u-pa buf) 0))
  (loop for o from 16 below 52 by 4 do (%t4u-w (+ va o) 0))   ; buffer pointers, high halves too
  (%t4u-w (+ va 8) (logior (if toggle (ash 1 31) 0) (ash bytes 16) (ash 3 10) (ash pid 8) #x80)))

(defun %t4u-qh-chars (addr ep mps speed &key control head (rl 0))
  (logior (ash rl 28) (if (and control (/= speed 2)) (ash 1 27) 0) (ash (logand mps #x7FF) 16)
          (if head (ash 1 15) 0) (if control (ash 1 14) 0) (ash speed 12) (ash ep 8) addr))

(defun %t4u-qh-caps (tthub ttport smask cmask)
  (logior (ash 1 30) (ash ttport 23) (ash tthub 16) (ash cmask 8) smask))

(defun %t4u-unlink-async ()
  "Take everything but the head out of the async ring, and wait for the controller to let go."
  (let ((h (%t4u-head-qh)))
    (%t4u-w h (logior (%t4u-pa h) 2))
    (%t4u-opw 0 (logior (%t4u-opr 0) (ash 1 6)))     ; interrupt on async advance
    (%t4u-wait (lambda () (logbitp 5 (%t4u-opr 4))) 20)
    (%t4u-opw 4 (ash 1 5))))

(defun t4-usb-control (addr speed tthub ttport mps0 rtype req value index len &optional out)
  "A control transfer.  IN data lands in the data page.  The bytes moved (>= 0), or -1."
  (let* ((in (logbitp 7 rtype))
         (s (%t4u-setup-buf)) (d (%t4u-data-buf))
         (qh (%t4u-ctl-qh)) (t0 (%t4u-ctl-td 0)) (t1 (%t4u-ctl-td 1)) (t2 (%t4u-ctl-td 2)))
    (setf (mem-ref s :u8) rtype (mem-ref (+ s 1) :u8) req
          (mem-ref (+ s 2) :u8) (logand value #xFF) (mem-ref (+ s 3) :u8) (ash value -8)
          (mem-ref (+ s 4) :u8) (logand index #xFF) (mem-ref (+ s 5) :u8) (ash index -8)
          (mem-ref (+ s 6) :u8) (logand len #xFF) (mem-ref (+ s 7) :u8) (ash len -8))
    (when (and out (not in)) (dotimes (i len) (setf (mem-ref (+ d i) :u8) (aref out i))))
    ;; SETUP, DATA (if any), STATUS (the other direction, toggle 1)
    (if (plusp len)
        (progn (%t4u-td t2 nil t 0 (if in 0 1) nil)
               (%t4u-td t1 t2 t len (if in 1 0) d)
               (%t4u-td t0 t1 nil 8 2 s))
        (progn (%t4u-td t2 nil t 0 1 nil)
               (%t4u-td t0 t2 nil 8 2 s)))
    (%t4u-w qh (logior (%t4u-pa (%t4u-head-qh)) 2))
    (%t4u-w (+ qh 4) (%t4u-qh-chars addr 0 mps0 speed :control t))
    (%t4u-w (+ qh 8) (%t4u-qh-caps (if (= speed 2) 0 tthub) (if (= speed 2) 0 ttport) 0 0))
    (%t4u-w (+ qh 12) 0)
    (%t4u-w (+ qh 16) (%t4u-pa t0))
    (%t4u-w (+ qh 20) 1)
    (loop for o from 24 below 68 by 4 do (%t4u-w (+ qh o) 0))
    (%t4u-w (%t4u-head-qh) (logior (%t4u-pa qh) 2))       ; into the ring
    (let* ((last t2)
           (done (%t4u-wait (lambda ()
                              (or (not (logbitp 7 (%t4u-r (+ last 8))))
                                  (logbitp 6 (%t4u-r (+ t0 8))) (logbitp 6 (%t4u-r (+ t1 8)))))
                            500))
           (bad (some (lambda (td) (let ((tok (%t4u-r (+ td 8)))) (and (logbitp 6 tok) tok)))
                      (if (plusp len) (list t0 t1 t2) (list t0 t2)))))
      (%t4u-unlink-async)
      (cond ((or (not done) bad) -1)
            ((and in (plusp len)) (- len (logand (ash (%t4u-r (+ t1 8)) -16) #x7FFF)))
            (t len)))))

;;; ---- the tree ---------------------------------------------------------------------------------

(defun %t4u-dev (addr) (find addr *t4-usb-devices* :key (lambda (d) (getf d :addr))))

(defun %t4u-ctl (dev rtype req value index len)
  (t4-usb-control (getf dev :addr) (getf dev :speed) (getf dev :tthub) (getf dev :ttport)
                  (getf dev :mps0) rtype req value index len))

(defun %t4u-string (dev index)
  "String descriptor INDEX of DEV (English), or NIL."
  (when (plusp index)
    (let ((n (%t4u-ctl dev #x80 6 (logior #x300 index) #x409 64)))
      (when (> n 2)
        (let ((len (min n (%t4u-byte (%t4u-data-buf) 0))))
          (coerce (loop for i from 2 below len by 2
                        for c = (%t4u-u16 (%t4u-data-buf) i)
                        collect (if (< 31 c 127) (code-char c) #\?))
                  'string))))))

(defvar *t4u-next-addr* 1)

(defun %t4u-identify (parent port speed tthub ttport)
  "The device just reset at address 0: address it and read what it is.  Its plist, or NIL."
  (let ((mps0 0) (probe (list :addr 0 :speed speed :tthub tthub :ttport ttport :mps0 8)))
    (dotimes (k 3)
      (when (>= (%t4u-ctl probe #x80 6 #x100 0 8) 8)
        (setq mps0 (max 8 (%t4u-byte (%t4u-data-buf) 7)))
        (return))
      (%t4u-sleep 20))
    (when (plusp mps0)
      (let ((addr *t4u-next-addr*))
        (setf (getf probe :mps0) mps0)
        (when (>= (%t4u-ctl probe #x00 5 addr 0 0) 0)
          (incf *t4u-next-addr*)
          (%t4u-sleep 10)
          (let ((dev (list :addr addr :parent parent :port port :speed speed
                           :tthub tthub :ttport ttport :mps0 mps0)))
            (when (>= (%t4u-ctl dev #x80 6 #x100 0 18) 18)
              (let ((b (%t4u-data-buf)))
                (setf (getf dev :id) (logior (ash (%t4u-u16 b 8) 16) (%t4u-u16 b 10))
                      (getf dev :class) (%t4u-byte b 4))
                (let ((iprod (%t4u-byte b 15)) (iman (%t4u-byte b 14)))
                  (setf (getf dev :name) (or (ignore-errors (%t4u-string dev iprod))
                                             (ignore-errors (%t4u-string dev iman)))))
                (setf *t4-usb-devices* (append *t4-usb-devices* (list dev)))
                (%t4u-say "~D: ~4,'0X:~4,'0X ~A (~[full~;low~;high~] speed, on ~D port ~D)"
                          addr (ash (getf dev :id) -16) (logand (getf dev :id) #xFFFF)
                          (or (getf dev :name) "?") speed parent port)
                dev))))))))

(defun %t4u-hub (hub)
  "HUB is a hub: configure it, power its ports and walk them."
  (when (and (>= (%t4u-ctl hub #x80 6 #x200 0 9) 9)
             (>= (%t4u-ctl hub #x00 9 (%t4u-byte (%t4u-data-buf) 5) 0 0) 0)
             (>= (%t4u-ctl hub #xA0 6 #x2900 0 8) 7))
    (let ((n (min 15 (%t4u-byte (%t4u-data-buf) 2)))
          (pwr (* 2 (%t4u-byte (%t4u-data-buf) 5))))
      (nconc hub (list :hub-ports n))                                  ; the same plist as the bus's
      (loop for p from 1 to n do (%t4u-ctl hub #x23 3 8 p 0))          ; PORT_POWER
      (%t4u-sleep (max 100 pwr))
      (loop for p from 1 to n
            do (when (> (%t4u-ctl hub #xA3 0 0 p 4) 0)
                 (when (logbitp 0 (%t4u-byte (%t4u-data-buf) 0))       ; connected
                   (%t4u-port hub p)))))))

(defun %t4u-port (hub p)
  "Reset port P of HUB and enumerate what is behind it."
  (%t4u-ctl hub #x23 3 4 p 0)                                          ; PORT_RESET
  (%t4u-sleep 60)
  (dotimes (k 10)
    (when (and (> (%t4u-ctl hub #xA3 0 0 p 4) 0)
               (not (logbitp 4 (%t4u-byte (%t4u-data-buf) 0))))        ; reset over
      (return))
    (%t4u-sleep 20))
  (let* ((st (%t4u-u16 (%t4u-data-buf) 0)))
    (%t4u-ctl hub #x23 1 20 p 0)                                       ; C_PORT_RESET
    (%t4u-ctl hub #x23 1 16 p 0)                                       ; C_PORT_CONNECTION
    (%t4u-sleep 20)
    (when (and (logbitp 0 st) (logbitp 1 st))
      (let* ((speed (cond ((logbitp 9 st) 1) ((logbitp 10 st) 2) (t 0)))
             (hub-hs (= (getf hub :speed) 2))
             (tthub (cond ((= speed 2) 0) (hub-hs (getf hub :addr)) (t (getf hub :tthub))))
             (ttport (cond ((= speed 2) 0) (hub-hs p) (t (getf hub :ttport))))
             (dev (%t4u-identify (getf hub :addr) p speed tthub ttport)))
        (when (and dev (= (getf dev :class) 9))
          (%t4u-hub dev))))))

(defun %t4u-root-port (p)
  "Reset root port P; enumerate the (high-speed) device behind it."
  (let ((sc (%t4u-portsc p)))
    (when (logbitp 0 (%t4u-r sc))
      (%t4u-w sc (logior (logand (%t4u-r sc) (lognot #x2E)) (ash 1 8)))   ; PR on, PE off
      (%t4u-sleep 50)
      (%t4u-w sc (logand (%t4u-r sc) (lognot (logior #x2A (ash 1 8)))))   ; PR off
      (%t4u-wait (lambda () (not (logbitp 8 (%t4u-r sc)))) 50)
      (%t4u-sleep 10)
      (if (logbitp 2 (%t4u-r sc))
          (let ((dev (%t4u-identify 0 p 2 0 0)))
            (when (and dev (= (getf dev :class) 9)) (%t4u-hub dev)))
          (%t4u-say "root port ~D: not high speed (no companion controller)" p)))))

;;; ---- HID: the first boot keyboard and mouse ------------------------------------------------------

(defvar *t4u-hid* (vector nil nil) "Bound keyboard and mouse: (dev ep mps iface), or NIL.")

(defun %t4u-hid-scan (dev)
  "DEV's boot keyboard / mouse interfaces: ((proto iface ep mps cfg) ...)."
  (let ((total (when (>= (%t4u-ctl dev #x80 6 #x200 0 9) 9) (%t4u-u16 (%t4u-data-buf) 2))))
    (when (and total (>= (%t4u-ctl dev #x80 6 #x200 0 (min total 1024)) 9))
      (let ((b (%t4u-data-buf)) (cfg (%t4u-byte (%t4u-data-buf) 5)) (pos 0) (iface nil) (proto 0) (out nil))
        (loop while (< pos (min total 1024))
              do (let ((len (%t4u-byte b pos)) (typ (%t4u-byte b (+ pos 1))))
                   (when (zerop len) (return))
                   (cond ((= typ 4)
                          (if (and (= (%t4u-byte b (+ pos 5)) 3) (= (%t4u-byte b (+ pos 6)) 1)
                                   (member (%t4u-byte b (+ pos 7)) '(1 2)))
                              (setq iface (%t4u-byte b (+ pos 2)) proto (%t4u-byte b (+ pos 7)))
                              (setq iface nil)))
                         ((and (= typ 5) iface (logbitp 7 (%t4u-byte b (+ pos 2)))
                               (= (logand (%t4u-byte b (+ pos 3)) 3) 3))
                          (push (list proto iface (logand (%t4u-byte b (+ pos 2)) #xF)
                                      (logand (%t4u-u16 b (+ pos 4)) #x7FF) cfg)
                                out)
                          (setq iface nil)))
                   (incf pos len)))
        (nreverse out)))))

(defun %t4u-arm (k)
  "Arm interrupt qTD K (0 keyboard, 1 mouse) for one report and hang it on its QH."
  (let ((h (aref *t4u-hid* k)) (td (%t4u-int-td k)) (qh (%t4u-int-qh k)))
    (%t4u-td td nil nil (min 64 (third h)) 1 (%t4u-int-buf k))
    ;; a halted transfer leaves the QH's overlay halted, and the controller then skips the QH:
    ;; clear it (the data toggle starts again at 0, as after a halt it must)
    (when (logbitp 6 (%t4u-r (+ qh 24))) (%t4u-w (+ qh 24) 0))
    (%t4u-w (+ qh 16) (%t4u-pa td))))

(defun %t4u-hid-bind ()
  "Bind the first boot keyboard and boot mouse; build the periodic schedule for them."
  (setf *t4u-hid* (vector nil nil))
  (dolist (dev *t4-usb-devices*)
    (unless (= (getf dev :class) 9)
      (dolist (i (ignore-errors (%t4u-hid-scan dev)))
        (destructuring-bind (proto iface ep mps cfg) i
          (let ((k (- proto 1)))
            (unless (aref *t4u-hid* k)
              (%t4u-ctl dev #x00 9 cfg 0 0)                      ; SET_CONFIGURATION
              (%t4u-ctl dev #x21 #x0B 0 iface 0)                 ; SET_PROTOCOL: boot
              (%t4u-ctl dev #x21 #x0A 0 iface 0)                 ; SET_IDLE: report on change
              (setf (aref *t4u-hid* k) (list dev ep mps iface))
              (%t4u-say "~[keyboard~;mouse~]: ~A (address ~D, endpoint ~D)" k
                        (or (getf dev :name) "?") (getf dev :addr) ep)))))))
  ;; the periodic schedule: every frame -> keyboard QH -> mouse QH
  (let ((chain 1))
    (loop for k from 1 downto 0
          do (let ((h (aref *t4u-hid* k)) (qh (%t4u-int-qh k)))
               (when h
                 (let* ((dev (first h)) (split (/= (getf dev :speed) 2)))
                   (%t4u-clear qh 128)
                   (%t4u-w qh chain)
                   (%t4u-w (+ qh 4) (%t4u-qh-chars (getf dev :addr) (second h) (third h) (getf dev :speed)))
                   (%t4u-w (+ qh 8) (%t4u-qh-caps (getf dev :tthub) (getf dev :ttport) 1 (if split #x1C 0)))
                   (%t4u-w (+ qh 20) 1)
                   (%t4u-arm k)
                   (setq chain (logior (%t4u-pa qh) 2))))))
    (dotimes (i 1024) (%t4u-w (+ (%t4u-frame-list) (* 4 i)) chain)))
  *t4u-hid*)

;;; ---- reports into events -----------------------------------------------------------------------

;; HID usage -> (plain . shifted) character codes for usages 4..56
(defparameter *t4u-usage-chars*
  (let ((v (make-array 57 :initial-element nil)))
    (loop for u from 4 to 29 for c from 97 do (setf (svref v u) (cons c (- c 32))))
    (loop for u from 30 for (p s) in '((#\1 #\!) (#\2 #\@) (#\3 #\#) (#\4 #\$) (#\5 #\%) (#\6 #\^)
                                       (#\7 #\&) (#\8 #\*) (#\9 #\() (#\0 #\)))
          do (setf (svref v u) (cons (char-code p) (char-code s))))
    (loop for (u p s) in '((44 #\Space #\Space) (45 #\- #\_) (46 #\= #\+) (47 #\[ #\{) (48 #\] #\})
                           (49 #\\ #\|) (51 #\; #\:) (52 #\' #\") (53 #\` #\~) (54 #\, #\<)
                           (55 #\. #\>) (56 #\/ #\?))
          do (setf (svref v u) (cons (char-code p) (char-code s))))
    v))

(defun %t4u-keysym (u mods)
  "The X11 keysym for usage U under modifier byte MODS, or NIL."
  (case u
    (40 #xff0d) (41 #xff1b) (42 #xff08) (43 #xff09) (76 #xffff) (73 #xff63)
    (74 #xff50) (75 #xff55) (77 #xff57) (78 #xff56)
    (79 #xff53) (80 #xff51) (81 #xff54) (82 #xff52)
    (t (cond ((<= 58 u 69) (+ #xffbe (- u 58)))
             ((and (< u 57) (svref *t4u-usage-chars* u))
              (let* ((e (svref *t4u-usage-chars* u))
                     (c (if (logtest mods #x22) (cdr e) (car e))))
                ;; Ctrl and a letter: the control code, as the PS/2 keyboard (t420.lisp) gives it
                (if (and (logtest mods #x11) (<= 97 (car e) 122)) (- (car e) 96) c)))))))

(defvar *t4u-keys-down* (make-array 8 :initial-element 0))
(defvar *t4u-repeat* nil "(keysym . next-time) of the key that repeats, or NIL.")
(defparameter *t4u-repeat-delay-ms* 500)
(defparameter *t4u-repeat-period-ms* 33)
(defvar *t4u-buttons* 0)

(defun %t4u-now-ms () (floor (get-internal-real-time) (floor internal-time-units-per-second 1000)))

(defun %t4u-key (ks)
  (setf *t4-events* (nconc *t4-events* (list (+ (* 4 1099511627776) ks)))))

(defun %t4u-kbd-report (b)
  (let ((mods (%t4u-byte b 0)) (newest nil))
    (loop for i from 2 to 7
          do (let ((u (%t4u-byte b i)))
               (when (and (> u 3) (not (find u *t4u-keys-down*)))
                 (let ((ks (%t4u-keysym u mods)))
                   (when ks (%t4u-key ks) (setq newest ks))))))
    (cond (newest (setq *t4u-repeat* (cons newest (+ (%t4u-now-ms) *t4u-repeat-delay-ms*))))
          ((loop for i from 2 to 7 never (> (%t4u-byte b i) 3)) (setq *t4u-repeat* nil)))
    (dotimes (i 8) (setf (aref *t4u-keys-down* i) (%t4u-byte b i)))))

(defun %t4u-repeat-tick ()
  (let ((r *t4u-repeat*))
    (when (and r (>= (%t4u-now-ms) (cdr r)))
      (%t4u-key (car r))
      (setf (cdr r) (+ (%t4u-now-ms) *t4u-repeat-period-ms*)))))

(defun %t4u-s8 (v) (if (> v 127) (- v 256) v))

(defun %t4u-mouse-report (b n)
  (let* ((btn (%t4u-byte b 0))
         ;; X11: 1 left 2 middle 4 right; a boot mouse: bit 0 left, 1 right, 2 middle
         (mask (logior (logand btn 1) (if (logbitp 2 btn) 2 0) (if (logbitp 1 btn) 4 0)))
         (dx (%t4u-s8 (%t4u-byte b 1))) (dy (%t4u-s8 (%t4u-byte b 2)))
         (wheel (if (> n 3) (%t4u-s8 (%t4u-byte b 3)) 0)))
    (setf *t4-px* (max 0 (min (- *t4-w* 1) (+ *t4-px* dx)))
          *t4-py* (max 0 (min (- *t4-h* 1) (+ *t4-py* dy))))          ; USB Y grows downward
    (setq *t4u-buttons* mask)
    (%t4-pointer-hide) (%t4-pointer-show)
    (flet ((ev (m) (setf *t4-events* (nconc *t4-events*
                                            (list (+ (* m 281474976710656)
                                                     (%t4-event 12 *t4-px* *t4-py*)))))))
      (ev mask)
      ;; the wheel: X11's buttons 4 (8) and 5 (16), pressed and let go
      (unless (zerop wheel)
        (let ((w (if (plusp wheel) 8 16)))
          (dotimes (i (min 5 (abs wheel))) (ev (logior mask w)) (ev mask)))))))

(defvar *t4u-reports* (vector 0 0) "Reports taken from the keyboard and the mouse.")
(defvar *t4u-errors* (vector 0 0) "Interrupt transfers that ended halted.")
(defvar *t4u-link-repairs* 0)

(defun %t4u-check-links ()
  "The periodic schedule's links as %T4U-HID-BIND made them.  A QH whose link reads 0 sends
   the controller to physical address 0 -- the real-mode interrupt table and the BIOS's data,
   which INT 13h then runs on -- and that was seen once (cause not found): repair and log."
  (let ((m (aref *t4u-hid* 1)) (k (aref *t4u-hid* 0)))
    (flet ((want (va v)
             (let ((now (%t4u-r va)))
               (unless (= now v)
                 (incf *t4u-link-repairs*)
                 (%t4u-say "periodic link at ~8,'0X was ~8,'0X, not ~8,'0X: repaired" va now v)
                 (%t4u-w va v)))))
      (when m (want (%t4u-int-qh 1) 1))
      (when k (want (%t4u-int-qh 0) (if m (logior (%t4u-pa (%t4u-int-qh 1)) 2) 1))))))

(defun t4-usb-poll ()
  "Collect what the bound keyboard and mouse reported since the last call, as desk events."
  (when *t4-usb*
    (%t4u-check-links)
    (dotimes (k 2)
      (let ((h (aref *t4u-hid* k)))
        (when h
          (let ((tok (%t4u-r (+ (%t4u-int-td k) 8))))
            (unless (logbitp 7 tok)                                   ; not active: done
              (if (logbitp 6 tok) (incf (aref *t4u-errors* k)) (incf (aref *t4u-reports* k)))
              (unless (logbitp 6 tok)                                 ; not halted
                (let ((n (- (min 64 (third h)) (logand (ash tok -16) #x7FFF))))
                  (when (plusp n)
                    (if (= k 0)
                        (%t4u-kbd-report (%t4u-int-buf 0))
                        (when (>= n 3) (%t4u-mouse-report (%t4u-int-buf 1) n))))))
              (%t4u-arm k))))))
    (%t4u-repeat-tick)))

;;; ---- bring-up -----------------------------------------------------------------------------------

(defun t4-usb-start ()
  "Take the controllers in *T4-USB-CONTROLLERS*, enumerate their buses, bind a keyboard and a
   mouse.  The devices found."
  (setf *t4-usb-devices* nil *t4u-next-addr* 1)
  (dolist (bdf *t4-usb-controllers*)
    ;; %T4U-TAKE binds *T4-USB* while it works and answers the plist: it is set here
    (when (setq *t4-usb* (ignore-errors (apply #'%t4u-take bdf)))
      (dotimes (p (getf *t4-usb* :ports))
        (ignore-errors (%t4u-root-port (+ p 1))))))
  (when *t4-usb* (ignore-errors (%t4u-hid-bind)))
  (length *t4-usb-devices*))
