;;;; t420-usb-disk.lisp -- the boot disk, read and written by the desk itself, not by the BIOS.
;;;;
;;;; The desk booted from a USB disk the BIOS drives (the T420's 00:1a.0 EHCI, Intel's rate-matching
;;;; hub on root port 1, the disk on its port 1), and went on reading and writing it through INT 13h:
;;;; the command channel, the log, test data.  That kept the BIOS's USB code alive in SMM, and it
;;;; keeps turning on the SMIs of the OTHER controller too (00:1d.0, USBLEGCTLSTS read #x1C, then
;;;; #xE03C straight after being cleared): with them on, a few hundred milliseconds of isochronous
;;;; OUT froze the machine.  Linux on the same machine owns both controllers, the BIOS's USB code
;;;; goes quiet, and USBLEGCTLSTS stays #xC0000000 through two seconds of playback.
;;;;
;;;; So the desk takes the boot disk's controller too: the same EHCI code as t420-usb.lisp (bound
;;;; to this controller's own schedules), the bus walked the same way, the first USB mass storage
;;;; device (class 8, SCSI, Bulk-Only) on it opened, and %T4-DISK-READ / %T4-DISK-WRITE
;;;; (t420.lisp) answered here: a CBW, the data straight to or from #x30000 (low memory, physical =
;;;; virtual, where the INT 13h path put it), a CSW.
;;;;
;;;; MEMORY: the net block's #x0F380000 .. #x0F38FFFF: the schedules as t420-usb.lisp lays them
;;;; out (frame list, async head, control QH and qTDs, the control data page), then from +#x4000
;;;; the two bulk QHs, their qTDs, the CBW and the CSW.

(defparameter *t4d-controller* '(0 #x1A 0) "PCI (bus dev fn) of the boot disk's EHCI.")
(defconstant +t4d-area+ #x0F380000)
(defvar *t4d* nil
  "The disk once taken: a plist (:ctl :devices :dev :in :out :mps :tag :blocks :ok :errors).")

(defmacro with-t4d-bus (&body body)
  `(let ((*t4-usb* (getf *t4d* :ctl)) (*t4u-area* +t4d-area+) (*t4-usb-devices* (getf *t4d* :devices)))
     ,@body))

(defun %t4d-mark (k)
  "Startup progress, for a camera: band K (8 pixels) across the top of the screen, green."
  (ignore-errors
    (loop for i from (* k 8 1366) below (* (+ k 1) 8 1366)
          do (modus-int::%mmio-write32 (+ #x0E000000 (* 4 i)) #x00C000))))

(defun %t4d-qh (dir) (%t4u-va (if (eq dir :in) #x4000 #x4080)))
(defun %t4d-td (k) (%t4u-va (+ #x4100 (* k 64))))
(defun %t4d-cbw () (%t4u-va #x4300))
(defun %t4d-csw () (%t4u-va #x4340))

;;; ---- the device ----------------------------------------------------------------------------------

(defun %t4d-msc-scan (dev)
  "DEV's Bulk-Only SCSI interface: (iface in-ep out-ep mps config), or NIL."
  (let ((n (%t4u-ctl dev #x80 6 #x200 0 255)) (b (%t4u-data-buf)) (i 0) (cfg 1) (iface nil) (in nil) (out nil) (mps 512))
    (when (plusp n)
      (setq cfg (%t4u-byte b 5))
      (loop while (< (+ i 1) n)
            do (let ((len (%t4u-byte b i)) (type (%t4u-byte b (+ i 1))))
                 (when (zerop len) (return))
                 (case type
                   (4 (if (and (null in) (= (%t4u-byte b (+ i 5)) 8) (= (%t4u-byte b (+ i 6)) 6)
                               (= (%t4u-byte b (+ i 7)) #x50))
                          (setq iface (%t4u-byte b (+ i 2)))
                          (when (and iface in out) (return))))
                   (5 (when (and iface (= 2 (logand (%t4u-byte b (+ i 3)) 3)))      ; bulk
                        (let ((a (%t4u-byte b (+ i 2))))
                          (setq mps (logand (%t4u-u16 b (+ i 4)) #x7FF))
                          (if (logbitp 7 a) (unless in (setq in (logand a 15))) (unless out (setq out (logand a 15))))))))
                 (incf i len))))
    (and iface in out (list iface in out mps cfg))))

(defun %t4d-qh-init (dir dev ep mps)
  (let ((qh (%t4d-qh dir)))
    (%t4u-clear qh 128)
    (%t4u-w (+ qh 4) (%t4u-qh-chars (getf dev :addr) ep mps (getf dev :speed) :rl 4))
    (%t4u-w (+ qh 8) (%t4u-qh-caps (getf dev :tthub) (getf dev :ttport) 0 0))
    (%t4u-w (+ qh 16) 1) (%t4u-w (+ qh 20) 1)))

(defun %t4d-ring ()
  "The async ring: head -> bulk IN -> bulk OUT -> head (a control transfer takes the ring over
   and leaves only the head: this puts the bulk QHs back)."
  (let ((h (%t4u-head-qh)) (i (%t4d-qh :in)) (o (%t4d-qh :out)))
    (%t4u-w o (logior (%t4u-pa h) 2))
    (%t4u-w i (logior (%t4u-pa o) 2))
    (%t4u-w h (logior (%t4u-pa i) 2))))

;;; ---- bulk transfers ------------------------------------------------------------------------------

(defun %t4d-fill-td (td next bytes pid pa)
  (%t4u-w td (if next (%t4u-pa next) 1))
  (%t4u-w (+ td 4) 1)
  (%t4u-w (+ td 12) pa)
  (loop for k from 1 to 4 do (%t4u-w (+ td 12 (* 4 k)) (+ (logand pa #xFFFFF000) (* 4096 k))))
  (loop for o from 32 below 52 by 4 do (%t4u-w (+ td o) 0))
  (%t4u-w (+ td 8) (logior (ash bytes 16) (ash 3 10) (ash pid 8) #x80)))

(defun %t4d-bulk (dir pa bytes)
  "Move BYTES (<= 32 KB, PA page-aligned past the first 16 KB) on the bulk pipe DIR.  The bytes
   moved, or NIL (halted or timed out)."
  (let* ((qh (%t4d-qh dir)) (pid (if (eq dir :in) 1 0))
         (two (> bytes 16384))
         (t0 (%t4d-td (if (eq dir :in) 0 2))) (t1 (%t4d-td (if (eq dir :in) 1 3))))
    (if two
        (progn (%t4d-fill-td t1 nil (- bytes 16384) pid (+ pa 16384))
               (%t4d-fill-td t0 t1 16384 pid pa))
        (%t4d-fill-td t0 nil bytes pid pa))
    (when (logbitp 6 (%t4u-r (+ qh 24))) (%t4u-w (+ qh 24) 0))         ; a halted overlay: clear
    (%t4u-w (+ qh 16) (%t4u-pa t0))
    (let* ((last (if two t1 t0))
           (done (%t4u-wait (lambda () (let ((a (%t4u-r (+ t0 8))) (b (%t4u-r (+ last 8))))
                                         (or (not (logbitp 7 b)) (logbitp 6 a) (logbitp 6 b))))
                            2000))
           (tds (if two (list t0 t1) (list t0))))
      (cond ((not done) (%t4u-w (+ qh 16) 1) nil)
            ((some (lambda (td) (logbitp 6 (%t4u-r (+ td 8)))) tds) nil)
            (t (- bytes (reduce #'+ (mapcar (lambda (td) (logand (ash (%t4u-r (+ td 8)) -16) #x7FFF)) tds))))))))

(defun %t4d-clear-halt (dir)
  (let ((d *t4d*))
    (%t4u-ctl (getf d :dev) #x02 1 0 (if (eq dir :in) (logior #x80 (getf d :in)) (getf d :out)) 0)
    (%t4u-w (+ (%t4d-qh dir) 24) 0)                                      ; toggle back to 0
    (%t4d-ring)))

(defun %t4d-scsi (cdb dir len)
  "One Bulk-Only command: CDB (a list of bytes), then LEN bytes of data at #x30000 in direction
   DIR (:in or :out), then the status.  T when the device says it went well."
  (let ((c (%t4d-cbw)) (tag (setf (getf *t4d* :tag) (logand (1+ (getf *t4d* :tag)) #xFFFFFFF))))
    (%t4u-w c #x43425355) (%t4u-w (+ c 4) tag) (%t4u-w (+ c 8) len)
    (%t4u-w (+ c 12) (logior (if (eq dir :in) #x80 0) (ash (length cdb) 16)))   ; flags, LUN 0, CB length
    (loop for o from 16 below 32 by 4 do (%t4u-w (+ c o) 0))
    (loop for b in cdb for i from 15 do (setf (mem-ref (+ c i) :u8) b))
    (and (eql (%t4d-bulk :out (%t4u-pa c) 31) 31)
         (or (zerop len)
             (%t4d-bulk dir #x30000 len)
             (progn (%t4d-clear-halt dir) t))                            ; a stalled data phase: the CSW says why
         (let ((s (%t4d-csw)))
           (%t4u-w s 0)
           (and (or (eql (%t4d-bulk :in (%t4u-pa s) 13) 13)
                    (progn (%t4d-clear-halt :in) (eql (%t4d-bulk :in (%t4u-pa s) 13) 13)))
                (= (%t4u-r s) #x53425355) (= (%t4u-r (+ s 4)) tag)
                (zerop (mem-ref (+ s 12) :u8)))))))

(defun %t4d-rw10 (op lba n)
  (list op 0 (ldb (byte 8 24) lba) (ldb (byte 8 16) lba) (ldb (byte 8 8) lba) (ldb (byte 8 0) lba)
        0 (ash n -8) (logand n #xFF) 0))

(defun t4d-read (lba nsec)
  "Read NSEC (<= 64) sectors at LBA to #x30000.  T when it worked."
  (with-t4d-bus
    (let ((ok (%t4d-scsi (%t4d-rw10 #x28 lba nsec) :in (* 512 nsec))))
      (if ok (incf (getf *t4d* :ok)) (incf (getf *t4d* :errors)))
      ok)))

(defun t4d-write (lba nsec)
  "Write NSEC (<= 64) sectors from #x30000 to LBA.  T when it worked."
  (with-t4d-bus
    (let ((ok (%t4d-scsi (%t4d-rw10 #x2A lba nsec) :out (* 512 nsec))))
      (if ok (incf (getf *t4d* :ok)) (incf (getf *t4d* :errors)))
      ok)))

;;; ---- taking it ---------------------------------------------------------------------------------

(defun t4d-take ()
  "Take the boot disk's controller from the BIOS, walk its bus, open the first mass storage
   device.  From here INT 13h on that disk is gone: the desk's disk is this.  The plist, or NIL."
  ;; the boot disk's first sector, as the BIOS reads it: the disk found below must be the same
  ;; one (on another PC the boot disk can be elsewhere, and this one somebody's stick)
  (let ((mbr (and (%t4-disk-read 0 1)
                  (let ((v (make-array 512 :element-type '(unsigned-byte 8))))
                    (dotimes (i 512 v) (setf (aref v i) (mem-ref (+ #x30000 i) :u8)))))))
    (unless mbr (return-from t4d-take nil))
    (%t4d-mark 0)
  (let ((*t4u-area* +t4d-area+) (*t4-usb-devices* nil) (*t4u-next-addr* 1) (*t4-usb* nil))
    (setq *t4-usb* (apply #'%t4u-take *t4d-controller*))
    (when *t4-usb*
      (%t4d-mark 1)
      (%t4d-mark 2)
      (dotimes (try 4)
        (setq *t4-usb-devices* nil *t4u-next-addr* 1)
        ;; a retry starts the controller over: at the desk's start the BIOS's handler runs AFTER
        ;; the handoff (the SMI on OS-ownership change) and undoes the setup -- measured, walk 0:
        ;; USBCMD 10030 (stopped), CONFIGFLAG 0, every PORTSC with bit 13 (handed to companion
        ;; controllers the T420 has none of); the reset that follows it sticks (walk 1: 4 devices)
        (when (plusp try)
          (let ((again (apply #'%t4u-take *t4d-controller*))) (when again (setq *t4-usb* again)))
          (%t4u-sleep 200))
        (dotimes (p (getf *t4-usb* :ports)) (ignore-errors (%t4u-root-port (+ p 1))))
        (t4-log "disk: walk ~D found ~D device~:P; PORTSC ~{~8,'0X~^ ~}; USBCMD ~8,'0X USBSTS ~8,'0X CONFIGFLAG ~X"
                try (length *t4-usb-devices*)
                (loop for p from 1 to (getf *t4-usb* :ports) collect (%t4u-r (%t4u-portsc p)))
                (%t4u-opr 0) (%t4u-opr 4) (%t4u-opr #x40))
        (when (some (lambda (d) (and (/= (getf d :class) 9) (ignore-errors (%t4d-msc-scan d)))) *t4-usb-devices*)
          (return))
        (%t4u-sleep 500))
      (dolist (dev *t4-usb-devices*)
        (unless (= (getf dev :class) 9)
          (let ((m (ignore-errors (%t4d-msc-scan dev))))
            (when m
              (destructuring-bind (iface in out mps cfg) m
                (%t4u-ctl dev #x00 9 cfg 0 0)                            ; SET_CONFIGURATION
                (%t4d-qh-init :in dev in mps)
                (%t4d-qh-init :out dev out mps)
                (%t4d-ring)
                (setq *t4d* (list :ctl *t4-usb* :devices *t4-usb-devices* :dev dev :iface iface
                                  :in in :out out :mps mps :tag 0 :ok 0 :errors 0))
                (%t4u-say "disk: ~A (address ~D, bulk ~D/~D, ~D bytes)" (or (getf dev :name) "?")
                          (getf dev :addr) in out mps)
                (return)))))))
    (when *t4d*
      (%t4d-mark 3)
      ;; TEST UNIT READY until the device has settled (a reset gadget can report a unit attention)
      (t4-log "disk: ready after ~A tries"
              (dotimes (k 50 :never)
                (when (with-t4d-bus (%t4d-scsi '(0 0 0 0 0 0) :in 0)) (return k))
                (%t4u-sleep 100)))
      ;; the desk's disk is this from here, if its first sector is the BIOS's
      (%t4d-mark 4)
      (if (and (dotimes (k 10 nil) (when (t4d-read 0 1) (return t)) (%t4u-sleep 100))
               (dotimes (i 512 t) (unless (= (mem-ref (+ #x30000 i) :u8) (aref mbr i)) (return nil))))
          (progn (setq *t4-disk-own* (cons #'t4d-read #'t4d-write)) (%t4d-mark 5))
          (%t4u-say "disk: ~A is not the boot disk, or unreadable (~D ok, ~D failed)"
                    (getf (getf *t4d* :dev) :name) (getf *t4d* :ok) (getf *t4d* :errors))))
    ;; the controller is ours either way now: if the BIOS can still reach its disk it is elsewhere,
    ;; but if nothing was found here the BIOS's own path may be gone too -- say so loudly
    (and *t4-disk-own* *t4d*))))
