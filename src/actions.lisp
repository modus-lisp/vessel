;;;; actions.lisp -- the Actions window: a grid of buttons, and a tap opens a Terminal and types the
;;;; button's form into it, as typing would, then presses Return to run it.  kiln-app-main (app.lisp)
;;;; registers it on the desk as "Actions".
;;;;
;;;; The list is *KILN-ACTIONS*: each entry is (LABEL FORM), FORM the text the Terminal evaluates.

(defparameter *kiln-actions*
  '(("ssh home"    "(ssh \"root@home.ynniv.com:54321\")")
    ("pubkey"      "(kiln-ios-ssh:ssh-pubkey)")
    ("tcp 1.1.1.1" "(conch::tcp-connect \"1.1.1.1\" 443)")
    ("time"        "(get-universal-time)")
    ("http"        "(subseq (seal.http:get-string \"http://example.com/\") 0 200)")
    ("https"       "(subseq (seal.http:get-string \"https://example.com/\") 0 200)")
    ("alert"       "(kiln-objc:objc-alert \"Hello\" \"from Lisp\")")
    ("storybook"   "(kiln-storybook:storybook)")
    ("tcp home"    "(conch::tcp-connect \"home.ynniv.com\" 54321)")
    ("tcp github"  "(conch::tcp-connect \"github.com\" 22)"))
  "Each button: its label, and the form a tap types into a new Terminal.")

(defun %kiln-actions-cells (w h n)
  "Button rectangles (X Y W H) for N buttons in a two-column grid over a W x H window.  Each
   button is two thirds of its grid cell, centred in it."
  (let* ((cols 2) (gap 8)
         (rows (max 1 (ceiling n cols)))
         (bw (floor (- w (* (1+ cols) gap)) cols))
         (bh (floor (- h (* (1+ rows) gap)) rows)))
    (loop for i below n
          collect (let ((cx (+ gap (* (mod i cols) (+ bw gap))))
                        (cy (+ gap (* (floor i cols) (+ bh gap)))))
                    (list (+ cx (floor bw 6)) (+ cy (floor bh 6))
                          (floor (* bw 2) 3) (floor (* bh 2) 3))))))

(defun %kiln-type-frames (form)
  "Queue FORM for a new Terminal, one step per frame: open it, type it, then draw and press Return.
   The form runs only once its line is on the screen."
  (let ((desk *kiln-desk*))
    (setf *kiln-frame-queue*
          (append *kiln-frame-queue*
                  (list (lambda () (glass.desk:desk-open desk "Terminal"))
                        (lambda () (loop for c across form
                                         do (glass.desk:desk-key desk t (char-code c))))
                        (lambda ()
                          (funcall *kiln-present-changes*)
                          (glass.desk:desk-key desk t #xff0d)))))))

(defun %kiln-actions-app (fb)
  "The Actions window's content: the buttons, and a press on one runs its form."
  (let* ((cells (%kiln-actions-cells (glass:fb-width fb) (glass:fb-height fb) (length *kiln-actions*)))
         (dirty t) (pressed nil) (was-down nil))
    (values nil
            (lambda (mask x y)
              (let ((down (logbitp 0 mask)))
                (cond ((and down (not was-down))
                       (setf was-down t)
                       (let ((i (position-if (lambda (c)
                                               (destructuring-bind (cx cy cw ch) c
                                                 (and (<= cx x) (< x (+ cx cw))
                                                      (<= cy y) (< y (+ cy ch)))))
                                             cells)))
                         (when i
                           (setf pressed i dirty t)
                           (%kiln-type-frames (second (nth i *kiln-actions*))))))
                      ((and (not down) was-down)
                       (setf was-down nil pressed nil dirty t)))))
            (lambda ()
              (when dirty
                (setf dirty nil)
                (glass:fb-fill fb #x10141a)
                (loop for entry in *kiln-actions*
                      for c in cells
                      for i from 0
                      do (destructuring-bind (x y bw bh) c
                           (glass:fb-rect fb x y bw bh (if (eql i pressed) #x4b6a94 #x2d4a6b))
                           (glass:fb-text fb (+ x 8) (+ y (floor bh 2) -8) (first entry)
                                          :size 16 :color #xe6ebf2)))
                t)))))
