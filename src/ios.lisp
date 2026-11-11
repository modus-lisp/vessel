;;;; ios.lisp — the iPhone app's entry point: glass on the phone's own screen.
;;;; The app itself is src/app.lisp; this is iOS's way to the screen.
;;;;
;;;; Loaded into the world `kiln ios' snapshots (kiln's boot/ios-world.lisp), so it is
;;;; native code by the time the phone runs it.  The app's modus.args say
;;;;   --core @kiln.core --eval (kiln-ios-main)
;;;; and this takes it from there: a glass framebuffer, the media player drawn
;;;; into it, touch turned into glass pointer events, and the picture copied to
;;;; the screen whenever the player repaints.  No VNC and no sockets: the
;;;; screen is the iOS shim's framebuffer (modus's host/ios/modus-ui.m),
;;;; reached through pseudo-syscalls 1001-1005.
;;;;
;;;; In CL-USER on purpose: it calls modus internals (%GC-SAFE-BLOCK-6,
;;;; %GC-WORD-OF, %CLI-COLLECT-ARGV), which a script names unqualified.

(defun %kiln-sys (n a b c &optional (d 0) (e 8))
  ;; E, bytes a pixel, is always 8 here: the iOS image does not pack (unsigned-byte 32) vectors
  (declare (ignore e))
  (modus-int::%gc-safe-block-6 n a b c d))

(defun %kiln-copy-text (text)
  "Put TEXT on the iPhone's pasteboard: pseudo-syscall 1007 reads the bytes in place and makes
   the UTF-8 string from them.  Characters past ASCII come out as ?, which is what a copy of a
   Terminal's text needs and no more."
  (let* ((v (map '(simple-array (unsigned-byte 8) (*))
                 (lambda (c) (if (< (char-code c) 128) (char-code c) 63)) text))
         (addr (+ (modus-int::%gc-word-of v (modus-int::%conv-addr #x100050A0)) 7)))
    (%kiln-sys 1007 addr (length v) 0)
    t))

(defun kiln-ios-main (&key autoplay)
  "The iPhone app (src/app.lisp), below the status bar, with the media player: the films beside
   the app (kiln ios copies them into its media/), and AUTOPLAY, one of them, playing at launch."
  ;; KILN_STORYBOOK=1 (simctl launch passes it as SIMCTL_CHILD_KILN_STORYBOOK) opens the storybook at start: the UI
  ;; suite's way in on any device, with no desk taps whose places depend on the screen
  ;; looked up when it runs: this file is loaded before the storybook's
  ;; the platform's voice and ear behind glass's Speak and Listen (speech.lisp)
  (when (find-package "KILN-SPEECH")
    (funcall (find-symbol "INSTALL-PLATFORM-SPEECH" "KILN-SPEECH")))
  (when (modus-int::%cli-getenv "KILN_STORYBOOK")
    (funcall (find-symbol "STORYBOOK" "KILN-STORYBOOK")))
  (kiln-app-main :autoplay autoplay :top 60 :media t))

(defun kiln-watch-main ()
  "The watch app: the storybook of the watch's components, over the desk's frame loop (which delivers their
   events).  The desk itself is glass at the phone's size; a watch screen shows its corner."
  (funcall (find-symbol "WATCH-STORYBOOK" "KILN-WATCH-STORYBOOK"))
  (kiln-app-main :top 0 :media nil))
