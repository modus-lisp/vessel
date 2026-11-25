;;;; windows/install.lisp -- vessel's apps in the Start menu (kiln windows runs this; it can also be run by hand):
;;;;
;;;;   vessel.exe --script vessel\windows\install.lisp
;;;;
;;;; For each app below: its App User Model ID registered for the current user, and a Start menu shortcut, named as
;;;; its window is titled, that runs VESSEL_INSTALL_EXE (else this executable) with the app's launcher from this
;;;; workspace -- the same ID and name the app itself uses for its toasts (win-host.lisp, TOAST-ID), so the shortcut
;;;; is the one its notifications show under.  Nothing here needs an administrator.

(defvar *workspace*
  ;; this file is WORKSPACE/vessel/windows/install.lisp: two directories up, as a plain C:/... path (a Start menu
  ;; shortcut shows it), whichever slashes it was named with
  (let* ((path (substitute #\/ #\\ (namestring *load-truename*)))
         (cut (position #\/ path :end (position #\/ path :end (position #\/ path :from-end t) :from-end t)
                                 :from-end t)))
    (subseq path 0 (1+ cut))))

(defun workspace-file (rel) (concatenate 'string *workspace* rel))
(defun windows-path (path) (substitute #\\ #\/ path))

(dolist (repo '("seal" "natrium" "cl-transport"))
  (push (workspace-file (concatenate 'string repo "/")) asdf:*central-registry*))
(asdf:load-system "seal")
(load (workspace-file "vessel/src/win-host.lisp"))
(vessel-windows:host-init)

(defparameter *apps*
  '(("Hello 3D"  "vessel/src/hello3d-windows.lisp")
    ("Storybook" "vessel/src/storybook-windows.lisp")
    ("chirp"     "vessel/src/chirp-windows.lisp"))
  "(window title, launcher): the title is the app's name in the Start menu and on its toasts.")

(let ((exe (let ((e (modus-int::%cli-getenv "VESSEL_INSTALL_EXE")))
             (if (and e (plusp (length e))) (windows-path e) nil)))
      (dir (string-right-trim "\\" (windows-path *workspace*))))
  (dolist (app *apps*)
    (destructuring-bind (name launcher) app
      (let ((id (vessel-windows::app-id name)))
        (vessel-windows::register-app-id id name)
        (format t "~&install: ~A (~A): ~A~%" name id
                (vessel-windows::start-menu-shortcut
                 id name :exe exe :dir dir :replace t
                 :args (format nil "--script \"~A\"" (workspace-file launcher))))))))
(finish-output)
