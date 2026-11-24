;;;; t420-settings.lisp -- the T420's Settings window: Display and USB.
;;;;
;;;; The Zero's Settings window (zero-display.lisp) sets its HDMI resolution, density and overscan
;;;; through the Pi's firmware; none of that is a PC's.  This is the same window -- the same look,
;;;; the same hit-tested buttons drawn with glass primitives -- over the T420's own facts:
;;;;   Display  the panel's native mode (or the BIOS's VBE one), the fitter, write-combining
;;;;   USB      each EHCI controller and who owns it (modus, or the BIOS: the boot disk's), the
;;;;            bus walked by t420-usb.lisp -- hubs, speeds, names -- the keyboard and mouse bound,
;;;;            the PS/2 keyboard and TrackPoint beside them, and Rescan.
;;;; Registered on the desk through app.lisp's *KILN-DESK-SETUP-HOOKS*, as the Zero's is.

(defparameter +t4s-bg+ #x10141a)
(defparameter +t4s-ink+ #xe6ebf2)
(defparameter +t4s-dim+ #x8b949e)
(defparameter +t4s-button+ #x2f3b4d)
(defparameter +t4s-accent+ #x5abe82)

(defun %t4s-ehci-controllers ()
  "Every EHCI controller on bus 0: ((dev fn id) ...)."
  (let ((out nil))
    (dotimes (dev 32)
      (dotimes (fn 8)
        (let ((id (ignore-errors (modus-int::pci-config-read 0 dev fn 0))))
          (when (and id (/= (logand id #xFFFF) #xFFFF)
                     (= (ash (modus-int::pci-config-read 0 dev fn 8) -8) #x0C0320))
            (push (list dev fn id) out)))))
    (nreverse out)))

(defun %t4s-speed (s) (case s (0 "full") (1 "low") (2 "high") (t "?")))

(defun %t4s-depth (dev)
  "How many hubs above DEV on its bus."
  (let ((d 0) (p (getf dev :parent)))
    (loop while (and p (plusp p))
          do (incf d)
             (setq p (let ((up (find p *t4-usb-devices* :key (lambda (x) (getf x :addr)))))
                       (and up (getf up :parent)))))
    d))

(defun t420-settings-app (fb)
  "The Settings window.  A desk make-fn."
  (let ((tab :usb) (hits '()) (dirty t) (was nil) (busy nil) (ctrls (ignore-errors (%t4s-ehci-controllers))))
    (labels ((text (x y s &key (size 13) (color +t4s-ink+) bold)
               (glass:fb-text fb x y s :size size :color color :font (glass:default-font bold)))
             (button (x y w h label kind &key on (size 13))
               (glass:fb-rect fb x y w h (if on +t4s-accent+ +t4s-button+))
               (text (+ x 10) (+ y (floor (- h size) 2) -1) label :size size
                     :color (if on +t4s-bg+ +t4s-ink+))
               (push (list kind x y w h) hits))
             (display-panel (y)
               (text 12 y "Panel" :size 13 :bold t)
               (text 112 y (format nil "~D x ~D, ~:[the BIOS's VBE mode~;native, the panel fitter off~]"
                                   *t4-w* *t4-h* *t4-native-on*))
               (incf y 22)
               (text 12 y "Framebuffer" :size 13 :bold t)
               (text 112 y (format nil "~D bytes a row, ~:[uncached~;write-combining (~:*~D pages)~]"
                                   *t4-pitch* *t4-wc-pages*))
               (incf y 22)
               (multiple-value-bind (ew eh) (ignore-errors (modus-int::%vbe-edid-size))
                 (text 12 y "EDID" :size 13 :bold t)
                 (text 112 y (if ew (format nil "~D x ~D preferred" ew eh) "none read")))
               (incf y 22)
               (text 12 y "Chrome" :size 13 :bold t)
               (text 112 y (format nil "~Dx (glass.wm density)" *kiln-wm-scale*)))
             (usb-panel (y)
               (text 12 y "Controllers" :size 13 :bold t)
               (incf y 20)
               (dolist (c ctrls)
                 (destructuring-bind (dev fn id) c
                   (let ((ours (and *t4-usb* (equal (getf *t4-usb* :bdf) (list 0 dev fn)))))
                     (text 24 y (format nil "EHCI 00:~2,'0X.~D  ~4,'0X:~4,'0X  ~A" dev fn
                                        (logand id #xFFFF) (ash id -16)
                                        (cond (ours "modus") ((member (list 0 dev fn) *t4-usb-controllers* :test #'equal)
                                                              "modus (not started)")
                                              (t "the BIOS (the boot disk)")))
                           :size 12 :color (if ours +t4s-ink+ +t4s-dim+))
                     (incf y 18))))
               (incf y 6)
               (text 12 y "Devices" :size 13 :bold t)
               (button 112 (- y 4) 84 22 (if busy "..." "Rescan") (list :rescan) :size 12)
               (incf y 22)
               (if (null *t4-usb-devices*)
                   (progn (text 24 y "none found" :size 12 :color +t4s-dim+) (incf y 18))
                   (dolist (d *t4-usb-devices*)
                     (let* ((role (cond ((and (aref *t4u-hid* 0) (eq d (first (aref *t4u-hid* 0)))) "keyboard")
                                        ((and (aref *t4u-hid* 1) (eq d (first (aref *t4u-hid* 1)))) "mouse")
                                        ((= (getf d :class) 9) (format nil "hub, ~D ports" (or (getf d :hub-ports) 0)))
                                        (t nil))))
                       (text (+ 24 (* 16 (%t4s-depth d))) y
                             (format nil "~A  ~4,'0X:~4,'0X  ~A speed~@[  -- ~A~]"
                                     (string-trim " " (or (getf d :name) "?"))
                                     (ash (or (getf d :id) 0) -16) (logand (or (getf d :id) 0) #xFFFF)
                                     (%t4s-speed (getf d :speed)) role)
                             :size 12 :color (if role +t4s-ink+ +t4s-dim+))
                       (incf y 18))))
               (incf y 6)
               (text 12 y "PS/2" :size 13 :bold t)
               (text 112 y (format nil "keyboard; TrackPoint / touchpad ~:[not answering~;on~]" *t4-mouse-ok*)
                     :size 12))
             (redraw ()
               (setf hits '())
               (glass:fb-fill fb +t4s-bg+)
               (button 12 10 90 24 "Display" (list :tab :display) :on (eq tab :display))
               (button 108 10 60 24 "USB" (list :tab :usb) :on (eq tab :usb))
               (if (eq tab :display) (display-panel 48) (usb-panel 48)))
             (press (x y)
               (let ((h (find-if (lambda (h) (destructuring-bind (k hx hy hw hh) h
                                               (declare (ignore k))
                                               (and (<= hx x (+ hx hw)) (<= hy y (+ hy hh)))))
                                 hits)))
                 (when h
                   (let ((k (first h)))
                     (case (first k)
                       (:tab (setf tab (second k)))
                       (:rescan
                        ;; shown as busy for one frame, then the walk (it blocks the desk a second or two)
                        (setf busy t)
                        (push (lambda ()
                                (ignore-errors (t4-usb-start))
                                (setf busy nil dirty t))
                              *kiln-frame-queue*))))
                   (setf dirty t)))))
      (values nil
              (lambda (mask x y)
                (let ((now (logbitp 0 mask)))
                  (when (and now (not was)) (press x y))
                  (setf was now)))
              (lambda () (when dirty (setf dirty nil) (redraw) t))
              nil nil
              (lambda (w h) (glass:fb-resize fb w h +t4s-bg+) (setf dirty t))
              nil))))

(defun %t4s-on-desk (desk dw dh)
  "app.lisp's desk-setup hook: Settings on the root menu."
  (%kiln-desk-register-app desk "Settings" #'t420-settings-app
                           :width (min dw 560) :height (min (- dh 60) 340)))

(pushnew '%t4s-on-desk *kiln-desk-setup-hooks*)
