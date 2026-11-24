;;;; t420-desk.lisp — loaded AFTER app.lisp: the i386 blit, and the desk's entry point.
;;;;
;;;; app.lisp's %KILN-BLIT-REGION passes the platform the address of the pixel vector's first
;;;; element as a 64-bit heap lays it out (%GC-WORD-OF + 7, 8 bytes a pixel).  On 32-bit x86 a
;;;; word is 4 bytes and the payload is elsewhere, so this replaces it with t420.lisp's
;;;; %T4-COPY: the native row copier (or, with no code area, AREF and MEM-REF).  The framebuffer is
;;;; uncached write-combining memory: written, never read (the pointer's background comes from
;;;; the desk's own framebuffer, t420.lisp).

(in-package :cl-user)

(defvar *t4-ip* nil "The address the network pipeline got at boot (four octets), or why not.")

(defvar *t4-beam-waits* 0 "Blits that waited for the scanout to leave their rows.")

(defun %t4-beam-clear (y rows)
  "TEAR-FREE: wait while the display is scanning rows Y .. Y+ROWS-1 (pipe A's scanline,
   PIPEADSL).  The copy that follows writes a row in a few microseconds and the scanout takes
   20.8 us a line, so a copy begun above or below the beam finishes before the beam reaches what
   it changed: the screen never shows half of a change.  At most a frame (a full-screen copy
   waits for vertical blank).  Intel display engine only (the native panel)."
  (when *t4-native-on*
    (let ((end (+ (get-internal-real-time) (floor internal-time-units-per-second 50))) (waited nil))
      (loop
        (let ((line (logand (t4-gpu-reg #x70000) #x1FFF)))
          (when (or (< line y) (>= line (+ y rows))
                    (>= (get-internal-real-time) end))
            (return))
          (setq waited t)))
      (when waited (incf *t4-beam-waits*)))))

(defun %kiln-blit-region (big k x0 y0 x y w h)
  "Show desk rectangle (X,Y,W,H), already magnified by K into BIG, at screen offset (X0,Y0)."
  (let* ((bw (glass:fb-width big)) (px (glass:fb-pixels big))
         (sx0 (+ x0 (* x k))) (sy0 (+ y0 (* y k)))
         (cols (min (* w k) (- *t4-w* sx0))) (rows (min (* h k) (- *t4-h* sy0))))
    (when (and (> cols 0) (> rows 0))
      (%t4-beam-clear sy0 rows)
      (%t4-copy px bw (* x k) (* y k) cols rows sx0 sy0)
      ;; the pointer, if the copy went over it
      (when (and *t4-under*
                 (< (- sx0 11) *t4-under-x* (+ sx0 cols)) (< (- sy0 16) *t4-under-y* (+ sy0 rows)))
        (%t4-pointer-show)))))

(defun kiln-t420-main (&key max-w max-h)
  "The desk on this PC's screen, until Ctrl-Alt-Del.  MAX-W / MAX-H bound the VBE mode.
   Answers :NO-VBE where there is no VBE (QEMU's -kernel boot has no BIOS shim)."
  ;; THE NETWORK FIRST, before the screen and the 8042 are touched: the T420's 82579 shares its
  ;; PHY with the management engine, which parks it when no driver claims it -- run minutes
  ;; after boot (from the desk's Terminal), the pipeline found a PHY reading all zeros over MDIO
  ;; and no link; run at boot, as the r-t420n image did, it got DHCP.  No cable: the desk comes
  ;; up once DHCP's tries run out.
  (%t4-console-to-log t)                 ; the console ring on: the pipeline's own messages
  (ignore-errors (t4-cmd-baseline))      ; commands written from now on run (t420.lisp)
  (let ((ip (handler-case
                (progn (modus-int::run-net-pipeline)
                       (let ((s (modus-int::e1000-state-base)))
                         (list (mem-ref (+ s 24) :u8) (mem-ref (+ s 25) :u8)
                               (mem-ref (+ s 26) :u8) (mem-ref (+ s 27) :u8))))
              (error (e) (list :error (princ-to-string e))))))
    (setf *t4-ip* ip)
    (t4-log "kiln t420: network ~S" ip)
    ;; the date: SNTP with an address, the CMOS clock without (t420.lisp)
    (t4-set-clock (and (integerp (first ip)) (plusp (first ip)))))
  (unless (%t4-screen-up max-w max-h)
    (return-from kiln-t420-main :no-vbe))
  (t4-gpu-dump "bios")
  (let ((native (and *t4-native* (%t4-native-panel))))
    (setf *t4-native-on* (and native t))
    (t4-log "kiln t420: screen ~Dx~D~:[ (VBE)~; (native, the panel fitter off)~]" *t4-w* *t4-h* native))
  (setf *t4-wc-pages* (%t4-write-combining))
  (t4-log "kiln t420: screen write-combining (~D pages)" *t4-wc-pages*)
  (%t4-mouse-init)
  ;; USB keyboards and mice (t420-usb.lisp): the controllers the BIOS does not boot from
  (when (fboundp 't4-usb-start)
    (t4-log "kiln t420: usb ~A" (handler-case (funcall 't4-usb-start) (error (c) (princ-to-string c)))))
  ;; the boot disk, from the BIOS (t420-usb-disk.lisp): its USB code then goes quiet in SMM, as it
  ;; does under Linux, instead of re-arming the other controller's SMIs
  (when (fboundp 't4d-take)
    (t4-log "kiln t420: disk ~A" (handler-case (if (funcall 't4d-take) "ours" "the BIOS's")
                                    (error (c) (princ-to-string c)))))
  (setf *t4-px* (floor *t4-w* 2) *t4-py* (floor *t4-h* 2))
  ;; the hardware cursor (t420-gpu.lisp), where there is an Intel display engine: the pointer
  ;; is then a plane the display blends, not pixels written into the screen and put back
  (when (and (fboundp 't4g-cursor-on) *t4-native-on*
             (ignore-errors (funcall 't4g-cursor-on *t4-px* *t4-py*)))
    (setf *t4-hw-pointer* (symbol-function 't4g-cursor-move))
    (t4-log "kiln t420: hardware cursor")
    ;; the blitter (t420-gpu.lisp): window moves copied on the screen by the GPU.  Only with the
    ;; hardware cursor -- a pointer drawn into the screen would be copied along with the window
    (if (ignore-errors (funcall 't4g-blitter-start))
        (progn (setf *kiln-show-move* (symbol-function 't4g-show-move))
               (t4-log "kiln t420: blitter ring running; window moves on the GPU"))
        (t4-log "kiln t420: no blitter")))
  (t4-log "kiln t420: desk ~Dx~D, mouse ~:[off~;on~]" *t4-w* *t4-h* *t4-mouse-ok*)
  (t4-log-flush)
  (unwind-protect
       (progn
         ;; glass.wm where the image has it (app.lisp %KILN-MAKE-DESK): a laptop's screen and
         ;; mouse, so desktop density (1x OPEN LOOK, not the phone's touch-sized 2x), and no
         ;; wireframe drags -- they are for a link that falls behind, and this screen is local
         (setq *kiln-wm-scale* 1)
         (let ((s (and (find-package "GLASS.WM") (find-symbol "*DRAG-ADAPTIVE*" "GLASS.WM"))))
           (when s (setf (symbol-value s) nil)))
         (kiln-app-main :top 0 :media nil))
    (%t4-console-to-log nil)
    (%t4-screen-down)))
