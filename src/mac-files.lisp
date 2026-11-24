;;;; mac-files.lisp -- warren, the column file browser, as a desk window on a Mac: the phone's files.lisp for a
;;;; computer.  The same entry point, KILN-FILES:FILES-APP, so the desk seats it as Files; but warren's own mouse and
;;;; wheel, as they are (no touch to make them of), and rooted at the person's home, not an app's Documents.
;;;;
;;;;   (reveal path)    the Files window -- opened if none is -- showing PATH: each folder down to it opened in its
;;;;                    column, PATH selected in the last (a folder opened, a file described).  A file dropped on the
;;;;                    desk comes here.

(defpackage #:kiln-files
  (:use #:cl)
  (:export #:files-app #:reveal))

(in-package #:kiln-files)

(defvar *app* nil "The open Files window's warren, or NIL.")
(defvar *window* nil "Its desk window.")

(defun files-app (fb)
  "The desk window: warren over FB, rooted at the home folder, driven by the mouse and the wheel."
  (let ((app (warren::make-browser :root (user-homedir-pathname)
                                   :width (glass:fb-width fb) :height (glass:fb-height fb))))
    (setf (warren::app-fb app) fb
          *app* app)
    (sb-thread:with-mutex ((warren::app-lock app)) (warren::paint app))
    (values
     (lambda (down keysym) (warren::on-key app down keysym))
     (lambda (mask x y) (warren::on-pointer app mask x y))
     (lambda () (warren::surface-take-dirty app))
     nil
     (lambda () (when (eq *app* app) (setf *app* nil *window* nil)))   ; closed
     nil)))

(defun %plain (p)
  "P's native name, without a trailing slash: how a folder and its entry are compared."
  (string-right-trim "/" (namestring p)))

(defun %select (app path)
  "In APP's last column, select the entry that is PATH; T if there was one."
  (let* ((cols (warren::app-columns app))
         (col (car (last cols)))
         (want (%plain path)))
    (when (and col (eq (warren::column-kind col) :dir))
      (let ((i (position want (warren::column-entries col)
                         :key (lambda (e) (%plain (warren::entry-path e))) :test #'string=)))
        (when i (warren::select-in-column app (1- (length cols)) i))))))

(defun %show (app path)
  "Walk APP down to PATH: re-rooted at PATH's own folder when it is not under the root, then each folder below the
   root selected in turn, and PATH last."
  (let* ((target (or (probe-file path) (pathname path)))
         (dir (directory-namestring target))
         (root (%plain (warren::app-root app))))
    (sb-thread:with-mutex ((warren::app-lock app))
      (unless (and (>= (length dir) (length root)) (string= root dir :end2 (length root)))
        ;; outside home: rooted where it is
        (setf (warren::app-root app) (pathname dir)
              (warren::app-columns app) (list (warren::make-dir-column (pathname dir)))
              root (%plain dir)))
      ;; the folders between the root and PATH, each opened
      (let ((rest (subseq dir (min (length dir) (1+ (length root))))) (at root))
        (loop for slash = (position #\/ rest)
              while slash
              do (setf at (concatenate 'string at "/" (subseq rest 0 slash))
                       rest (subseq rest (1+ slash)))
                 (%select app at)))
      (%select app target)
      (setf (warren::app-dirty app) t))))

(defun reveal (path)
  "The Files window showing PATH, opened if need be."
  (let ((desk cl-user::*kiln-desk*))
    (unless *app*
      (setf *window* (cl-user::%kiln-desk-open desk "Files")))
    (when *app* (%show *app* path))
    (when (and *window* (cl-user::%wm-desk-p desk))
      (ignore-errors (cl-user::%wm-call "WM-RAISE" desk *window*)))
    *app*))
