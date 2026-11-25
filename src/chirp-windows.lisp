;;;; chirp-windows.lisp -- chirp's chat (chirp/src/app.lisp: bitchat's public channel over Bluetooth LE) as the
;;;; phone shows it, on Windows: modus.exe, win-host.lisp's native app layer, modus's WinRT radio.
;;;;
;;;;   modus.exe --script vessel\src\chirp-windows.lisp          (from the workspace)
;;;;
;;;; CHIRP_TOUR=DIR: the chat, a message posted from the compose bar, then Nearby, the Radio Log and Nickname, each
;;;; pictured as DIR/chirp-N-what.bmp; then it quits.

(defvar *workspace*
  (let ((w (modus-int::%cli-getenv "VESSEL_WORKSPACE")))
    (if (and w (> (length w) 0))
        (if (member (char w (1- (length w))) '(#\/ #\\)) w (concatenate 'string w "/"))
        (namestring (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))))))

(defun workspace-file (rel) (concatenate 'string *workspace* rel))

(dolist (repo '("seal" "natrium" "cl-transport" "cram" "chirp"))
  (push (workspace-file (concatenate 'string repo "/")) asdf:*central-registry*))
(asdf:load-system "seal")

(defvar *kiln-frame-hooks* nil)
(defvar *term-cursor* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defun decode-ptr-addr ()
  (+ (modus-int::%gc-word-of *term-cursor* (modus-int::%conv-addr #x100050A0)) 7))
(load (workspace-file "modus/net/term.lisp"))
(load (workspace-file "vessel/src/win-host.lisp"))

(unless (equal (modus-int::%cli-getenv "VESSEL_GC") "plain")
  (ignore-errors (modus:gc-tenure :promote nil)))

(vessel-windows:open-window "chirp" 400 860)
(load (workspace-file "vessel/src/ios-swift.lisp"))
(vessel-windows:install-bridge)
(vessel-windows:load-widgets)
(load (workspace-file "vessel/src/ios-native.lisp"))
(asdf:load-system "chirp/app")

(chirp-app:open-chat)
(format t "~&chirp: open~%")
(finish-output)

(defun frames (n)
  (dotimes (i n)
    (cl-user::%kiln-sys 1008 34 0 0)
    (dolist (hook *kiln-frame-hooks*)
      (handler-case (funcall hook) (error (e) (format t "~&chirp: ~A~%" e))))))

(let ((dir (modus-int::%cli-getenv "CHIRP_TOUR")))
  (when (and dir (plusp (length dir)))
    (flet ((shot (n what)
             (frames 20)
             (vessel-windows::window-shot vessel-windows:*hwnd* (format nil "~A/chirp-~D-~A.bmp" dir n what))
             (format t "~&chirp: pictured ~A~%" what) (finish-output)))
      (frames 30)
      (shot 1 "chat")
      ;; a message, typed into the compose bar and sent with its button
      (let* ((compose (loop for h being the hash-values of vessel-windows::*ap-composes* return h))
             (edit (and compose (vessel-windows::ctl-hwnd (vessel-windows::ctl-of compose)))))
        (when edit
          (vessel-windows::u vessel-windows::*send-message* edit #x0C 0 (vessel-windows::wide1 "hello from Windows"))
          (frames 5)
          (vessel-windows::ap-compose-send compose)                               ; Send, as its click does
          (frames 10)
          (format t "~&chirp: rows now ~D~%" (length chirp-app::*rows*))))
      (shot 2 "sent")
      (chirp-app::people) (shot 3 "nearby")
      (vessel-windows::ap-pop (vessel-windows::ap-nav-of chirp-app::*nav*))
      (frames 10)
      (chirp-app::radio-log) (shot 4 "radio")
      (vessel-windows::ap-pop (vessel-windows::ap-nav-of chirp-app::*nav*))
      (frames 10)
      (chirp-app::nickname) (shot 5 "nickname"))
    (format t "~&chirp: tour done~%")
    (finish-output)
    (sb-ext:exit :code 0)))

(loop (frames 1))
