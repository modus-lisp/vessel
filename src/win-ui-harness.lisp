;;;; win-ui-harness.lisp -- a test bench for the Windows widgets (win-host.lisp and the win-ui-*.lisp files on top of
;;;; it): the same bridge an app uses, without an app.
;;;;
;;;;   modus.exe --script your-test.lisp      where your-test.lisp begins
;;;;     (load "C:/Users/xbox/Documents/modus-lisp/vessel/src/win-ui-harness.lisp")
;;;;     (load-widgets "win-ui-controls")     ; your file(s), from vessel/src, after win-host
;;;;
;;;; then calls KILN-SWIFT:UI-* exactly as an app would (the bridge reaches win-host's DEFSWIFTs), and checks what
;;;; came back with these:
;;;;   (settle &optional (seconds 0.3))   pump the window's messages that long: what the UI thread did reaches Lisp
;;;;   (events)                           the events since the last call, as (kind handle value) terms, and clears them
;;;;   (hwnd-of h)                        a handle's native window
;;;;   (send h msg wp lp)                 SendMessageW to a handle's window, on the windows' thread: its result
;;;;   (check name ok)                    print PASS or FAIL NAME; count them
;;;;   (summary)                          "N passed, M failed"
;;;; A test may also post messages to a control (PostMessageW) to stand in for a click, a keystroke, a selection.

(defvar *workspace* "C:/Users/xbox/Documents/modus-lisp/")
(dolist (repo '("seal" "natrium" "cl-transport"))
  (push (concatenate 'string *workspace* repo "/") asdf:*central-registry*))
(asdf:load-system "seal")

(defvar *kiln-frame-hooks* nil)
(defvar *term-cursor* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defun decode-ptr-addr ()
  (+ (modus-int::%gc-word-of *term-cursor* (modus-int::%conv-addr #x100050A0)) 7))
(load (concatenate 'string *workspace* "modus/net/term.lisp"))
(load (concatenate 'string *workspace* "vessel/src/win-host.lisp"))
(vessel-windows:open-window "Widgets" 900 700)
(load (concatenate 'string *workspace* "vessel/src/ios-swift.lisp"))
(vessel-windows:install-bridge)

(defvar *widgets-loaded* '())
(defun load-widgets (&rest names)
  "NAMES (win-ui-controls ...), from vessel/src, each once.  VESSEL_ALL_WIDGETS set: every widget file, so a test
   also shows that the files work together."
  (dolist (n (if (plusp (length (or (modus-int::%cli-getenv "VESSEL_ALL_WIDGETS") "")))
                 '("win-ui-controls" "win-ui-choices" "win-ui-containers" "win-ui-nav" "win-ui-gestures")
                 names))
    (unless (member n *widgets-loaded* :test #'string=)
      (push n *widgets-loaded*)
      (load (concatenate 'string *workspace* "vessel/src/" n ".lisp")))))

(defun settle (&optional (seconds 0.3))
  (loop repeat (max 1 (round seconds 0.02))
        do (sleep 0.02) (vessel-windows::pump) (vessel-windows::run-timers)
           (dolist (f vessel-windows::*tick-hooks*) (funcall f))))

(defun events ()
  "The events since the last call (but the window's own :appstate), oldest first; cleared."
  (prog1 (remove :appstate vessel-windows::*ui-events* :key #'first) (setf vessel-windows::*ui-events* nil)))

(defun hwnd-of (h) (vessel-windows::ctl-hwnd (vessel-windows::ctl-of h)))

(defun send (h msg wp lp) (vessel-windows::u vessel-windows::*send-message* (hwnd-of h) msg wp lp))

(defvar *passed* 0)
(defvar *failed* 0)
(defun check (name ok)
  (if ok (incf *passed*) (incf *failed*))
  (format t "~&~:[FAIL~;PASS~] ~A~%" ok name)
  (finish-output)
  ok)
(defun summary () (format t "~&~D passed, ~D failed~%" *passed* *failed*) (finish-output))
