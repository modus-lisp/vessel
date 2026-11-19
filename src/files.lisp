;;;; files.lisp -- warren, the Finder-style column file browser, as a desk window on the phone.
;;;;
;;;; warren draws into a framebuffer its host owns (WARREN:DESKTOP-SURFACE's contract: on-key, on-pointer,
;;;; dirty-p), so the desk can seat it like any window.  What a phone adds:
;;;;
;;;;   TOUCH.  warren is driven by a mouse: a click selects, a wheel scrolls.  A finger here is both: a drag
;;;;   up or down scrolls the column under it (wheel steps), sideways pans the columns, and a press that
;;;;   does not move is the click.
;;;;
;;;;   OPENING.  A press held on a file opens it in Quick Look (iOS's own viewer, with its share button):
;;;;   pictures, PDFs, text, Markdown, films.  warren itself only describes a file.
;;;;
;;;;   PREVIEWS.  warren decodes a picture on a thread of its own; on modus that thread may not store into
;;;;   the app's tables, and a phone-sized photo is more than its heap.  iOS makes the thumbnail instead
;;;;   (WARREN:*PREVIEW-FN*, KILN-NATIVE:APP-THUMBNAIL).
;;;;
;;;; Rooted at Documents/: what the Files app shows as this app's folder -- the agent's chats, pictures and
;;;; working files among it.

(defpackage #:kiln-files
  (:use #:cl)
  (:export #:files-app))

(in-package #:kiln-files)

(defparameter +slop+ 8 "Pixels a press may move and still be a tap.")
(defparameter +wheel-px+ 24 "Pixels of drag per wheel step (warren scrolls three rows a step).")

(defun %native-preview (path box-w box-h)
  "WARREN:*PREVIEW-FN* on the phone: iOS's thumbnail of PATH as (values rgba w h)."
  (multiple-value-bind (w h px)
      (kiln-native:app-thumbnail (namestring path) (max box-w box-h) :background #x1e1e1e)
    (when w
      (let ((rgba (make-array (* 4 w h) :element-type '(unsigned-byte 8))))
        (dotimes (i (* w h))
          (let ((p (aref px i)) (j (* 4 i)))
            (setf (aref rgba j) (ldb (byte 8 16) p)
                  (aref rgba (+ j 1)) (ldb (byte 8 8) p)
                  (aref rgba (+ j 2)) (ldb (byte 8 0) p)
                  (aref rgba (+ j 3)) 255)))
        ;; a box-fitting thumbnail may still be larger than the box on one side: warren scales it down
        (values rgba w h)))))

(defun %entry-at (app x y)
  "The warren entry under (X,Y), or NIL."
  (multiple-value-bind (ci ri)
      (warren::hit-test (warren::app-columns app) (warren::app-hscroll app)
                        (warren::app-vw app) (warren::app-vh app) x y)
    (when ci
      (let ((col (nth ci (warren::app-columns app))))
        (and col ri (nth ri (warren::column-entries col)))))))

(defun files-app (fb)
  "The desk window: warren over FB, rooted at Documents/."
  (setf warren:*preview-fn* #'%native-preview)
  (let* ((root (pathname (concatenate 'string (string-right-trim "/" (kiln-native:app-documents)) "/")))
         (app (warren::make-browser :root root :width (glass:fb-width fb) :height (glass:fb-height fb)))
         (press nil))                    ; (x0 y0 t0 dragged last-y last-x) while a finger is down
    (setf (warren::app-fb app) fb)
    (sb-thread:with-mutex ((warren::app-lock app)) (warren::paint app))
    (flet ((wheel (mask x y)
             (warren::on-pointer app mask x y)
             (warren::on-pointer app 0 x y)))
      (values
       (lambda (down keysym) (warren::on-key app down keysym))
       (lambda (mask x y)
         (cond
           ((logtest mask 1)
            (if (null press)
                (setf press (list x y (get-internal-real-time) nil y x))
                (destructuring-bind (x0 y0 t0 dragged ly lx) press
                  (declare (ignore t0))
                  (when (or dragged (> (max (abs (- x x0)) (abs (- y y0))) +slop+))
                    (setf (fourth press) t)
                    ;; a finger moving up shows what is below: the wheel's down
                    (loop while (>= (- ly y) +wheel-px+) do (wheel 16 x y) (decf ly +wheel-px+))
                    (loop while (>= (- y ly) +wheel-px+) do (wheel 8 x y) (incf ly +wheel-px+))
                    (loop while (>= (- lx x) +wheel-px+) do (wheel 64 x y) (decf lx +wheel-px+))
                    (loop while (>= (- x lx) +wheel-px+) do (wheel 32 x y) (incf lx +wheel-px+))
                    (setf (fifth press) ly (sixth press) lx)))))
           (press
            (destructuring-bind (x0 y0 t0 dragged ly lx) press
              (declare (ignore ly lx))
              (setf press nil)
              (unless dragged
                (if (>= (- (get-internal-real-time) t0) (* 0.45 internal-time-units-per-second))
                    ;; held: open a file in Quick Look
                    (let ((e (%entry-at app x0 y0)))
                      (if (and e (not (warren::entry-dir-p e)))
                          (kiln-native:app-preview (namestring (warren::entry-path e)))
                          (progn (warren::on-pointer app 1 x0 y0) (warren::on-pointer app 0 x0 y0))))
                    (progn (warren::on-pointer app 1 x0 y0) (warren::on-pointer app 0 x0 y0))))))))
       (lambda () (warren::surface-take-dirty app))
       nil nil nil))))
