;;;; t420-desk.lisp — loaded AFTER app.lisp: the i386 blit, and the desk's entry point.
;;;;
;;;; app.lisp's %KILN-BLIT-REGION passes the platform the address of the pixel vector's first
;;;; element as a 64-bit heap lays it out (%GC-WORD-OF + 7, 8 bytes a pixel).  On 32-bit x86 a
;;;; word is 4 bytes and the payload is elsewhere, so this replaces it with a copy that reads
;;;; the pixels through AREF and writes the framebuffer through MEM-REF.  The framebuffer is
;;;; uncached write-combining memory: written, never read (the pointer's background comes from
;;;; the desk's own framebuffer, t420.lisp).

(in-package :cl-user)

(defun %kiln-blit-region (big k x0 y0 x y w h)
  "Show desk rectangle (X,Y,W,H), already magnified by K into BIG, at screen offset (X0,Y0)."
  (let* ((bw (glass:fb-width big)) (px (glass:fb-pixels big))
         (sx0 (+ x0 (* x k))) (sy0 (+ y0 (* y k)))
         (cols (min (* w k) (- *t4-w* sx0))) (rows (min (* h k) (- *t4-h* sy0))))
    (when (and (> cols 0) (> rows 0))
      (dotimes (r rows)
        (let ((src (+ (* (+ (* y k) r) bw) (* x k)))
              (dst (%t4-px-addr sx0 (+ sy0 r))))
          (dotimes (c cols)
            (setf (mem-ref dst :u32) (aref px src))
            (setq src (+ src 1) dst (+ dst 4)))))
      ;; the pointer, if the copy went over it
      (when (and *t4-under*
                 (< (- sx0 11) *t4-under-x* (+ sx0 cols)) (< (- sy0 16) *t4-under-y* (+ sy0 rows)))
        (%t4-pointer-show)))))

(defun kiln-t420-main (&key max-w max-h)
  "The desk on this PC's screen, until Ctrl-Alt-Del.  MAX-W / MAX-H bound the VBE mode.
   Answers :NO-VBE where there is no VBE (QEMU's -kernel boot has no BIOS shim)."
  (unless (%t4-screen-up max-w max-h)
    (return-from kiln-t420-main :no-vbe))
  (%t4-mouse-init)
  (setf *t4-px* (floor *t4-w* 2) *t4-py* (floor *t4-h* 2))
  (unwind-protect
       (kiln-app-main :top 0 :media nil)
    (%t4-screen-down)))
