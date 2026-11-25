;;;; storybook-windows.lisp -- the storybook (ios-storybook.lisp: every native component, a story at a time) on
;;;; Windows, run by modus.exe:
;;;;
;;;;   modus.exe --script vessel\src\storybook-windows.lisp          (from the workspace)
;;;;
;;;; A phone-shaped window; win-host.lisp and its widget files (win-ui-*.lisp) answer the components' calls.
;;;; VESSEL_WORKSPACE names the workspace, else it is the directory above vessel.  STORYBOOK_STORY opens at a story
;;;; by name ("list", "date" ...).

(defvar *workspace*
  (let ((w (modus-int::%cli-getenv "VESSEL_WORKSPACE")))
    (if (and w (> (length w) 0))
        (if (member (char w (1- (length w))) '(#\/ #\\)) w (concatenate 'string w "/"))
        (namestring (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))))))

(defun workspace-file (rel) (concatenate 'string *workspace* rel))

(dolist (repo '("seal" "natrium" "cl-transport"))
  (push (workspace-file (concatenate 'string repo "/")) asdf:*central-registry*))
(asdf:load-system "seal")

;; what the Mac world defines for the bridges: the frame hooks, and the term decoder's cursor
(defvar *kiln-frame-hooks* nil)
(defvar *term-cursor* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defun decode-ptr-addr ()
  (+ (modus-int::%gc-word-of *term-cursor* (modus-int::%conv-addr #x100050A0)) 7))
(load (workspace-file "modus/net/term.lisp"))
(load (workspace-file "vessel/src/win-host.lisp"))

(unless (equal (modus-int::%cli-getenv "VESSEL_GC") "plain")
  (ignore-errors (modus:gc-tenure :promote nil)))

;; an iPhone's screen, in points: the stories are laid out for it
(vessel-windows:open-window "Storybook" 400 860)
(load (workspace-file "vessel/src/ios-swift.lisp"))
(vessel-windows:install-bridge)
(vessel-windows:load-widgets)

;; THE VOICE PROTOCOL, a stand-in.  speech.lisp puts the platform's voice and ear behind glass's voice protocol
;; (glass.desk's VOICE-SAY, EAR-START ...), which is not part of this checkout of glass; the storybook's speech story
;; needs only those generic functions and the two variables, so they are defined here.  A real glass loaded first
;; is used instead.
(unless (find-package "GLASS.DESK")
  (defpackage #:glass.desk
    (:use #:cl)
    (:export #:*voice* #:*ear* #:voice-say #:voice-hush #:voice-state #:voice-report
             #:ear-start #:ear-stop #:ear-clear #:ear-state #:ear-report #:ear-text #:ear-level))
  (eval (read-from-string
         "(progn (defvar glass.desk:*voice* nil) (defvar glass.desk:*ear* nil)
            (dolist (f '(glass.desk:voice-say)) (eval `(defgeneric ,f (v text))))
            (dolist (f '(glass.desk:voice-hush glass.desk:voice-state glass.desk:voice-report
                         glass.desk:ear-start glass.desk:ear-stop glass.desk:ear-clear glass.desk:ear-state
                         glass.desk:ear-report glass.desk:ear-text glass.desk:ear-level))
              (eval `(defgeneric ,f (x)))))")))

(dolist (f '("gpu" "camera" "microphone" "speech" "now-playing" "ios-storybook"))
  (load (workspace-file (concatenate 'string "vessel/src/" f ".lisp"))))
(kiln-speech:install-platform-speech)

(let ((story (modus-int::%cli-getenv "STORYBOOK_STORY")))
  (kiln-storybook:storybook (and story (plusp (length story)) story)))
(format t "~&storybook: open~%")
(finish-output)

(defun frames (n)
  "N refreshes of the app's loop."
  (dotimes (i n)
    (cl-user::%kiln-sys 1008 34 0 0)
    (dolist (hook *kiln-frame-hooks*) (ignore-errors (funcall hook)))))

;; STORYBOOK_TOUR=DIR: every story of every page shown in turn and pictured (VESSEL-WINDOWS::WINDOW-SHOT) as
;; DIR/page-number-name.bmp, then the storybook quits.  For looking at all of it without clicking through it.
(let ((dir (modus-int::%cli-getenv "STORYBOOK_TOUR")))
  (when (and dir (plusp (length dir)))
    (frames 30)
    (loop for page from 1 to (length kiln-storybook::*pages*)
          do (kiln-storybook::%show-page page)
             (loop for (name . nil) in (kiln-storybook::%page-stories page) for k from 1
                   do (handler-case
                          (progn (kiln-storybook::%show name)
                                 (frames 20)
                                 (vessel-windows::window-shot vessel-windows:*hwnd*
                                                              (format nil "~A/~D-~2,'0D-~A.bmp" dir page k name))
                                 (format t "~&storybook: pictured ~D ~A~%" page name))
                        (error (e) (format t "~&storybook: ~A failed: ~A~%" name e)))
                      (finish-output)))
    (format t "~&storybook: tour done~%")
    (finish-output)
    (sb-ext:exit :code 0)))

;; the app's loop: the next refresh, then the frame hooks (SWIFT-PUMP delivers the components' events)
(loop
  (cl-user::%kiln-sys 1008 34 0 0)
  (dolist (hook *kiln-frame-hooks*)
    (handler-case (funcall hook)
      (error (e) (format t "~&storybook: ~A~%" e)))))
