;;;; watch.lisp -- the watch app on a real watch: the storybook of its native components, and nothing else.
;;;;
;;;; A watch (arm64_32: 4 GB of address space, ~300 MB of memory) carries a world of its own, kiln's
;;;; boot/watch-world.lisp: modus, the term codec, the Swift bridge (ios-swift.lisp) and the watch storybook --
;;;; no desk, no glass, no media.  So no desk frame loop either: this file is the little of app.lisp and
;;;; ios.lisp the bridge needs -- the pseudo-syscall, the frame hooks -- and a loop that runs them.
;;;;
;;;; The app's modus.args say  --core @kiln.core --eval (kiln-watch-run)

(defun %kiln-sys (n a b c &optional (d 0) (e 8))
  "Pseudo-syscall N (ios.lisp's): the bridge calls Swift through 1060."
  (declare (ignore e))
  (modus-int::%gc-safe-block-6 n a b c d))

(defvar *kiln-frame-hooks* nil
  "Functions run each frame (app.lisp's): the bridge's event pump puts itself here.")

(defvar *kiln-frame-seconds* 1/30
  "How long a frame of the watch loop sleeps: the events wait at most this long.")

(defun kiln-watch-run ()
  "The watch app: open the storybook, then deliver its events, a frame at a time, for good."
  (funcall (find-symbol "WATCH-STORYBOOK" "KILN-WATCH-STORYBOOK"))
  (loop
    (dolist (f *kiln-frame-hooks*)
      (handler-case (funcall f)
        (error (e) (format t "~&watch: frame hook ~A: ~A~%" f e))))
    (sleep *kiln-frame-seconds*)))
