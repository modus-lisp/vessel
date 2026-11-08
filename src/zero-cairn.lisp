;;;; zero-cairn.lisp — git on the card: cairn's store and working tree on the Zero's FAT32 store.
;;;;
;;;; The card's second partition (zero-card.lisp MAKE-STORE, mounted by %STORE) holds the
;;;; workspace a board syncs into: /src/<repo>, each a real git checkout, .git and all.  This
;;;; gives cairn backends for it, so cairn clones straight onto the card:
;;;;
;;;;   (kiln-zero-card:clone-to-store "https://github.com/modus-lisp/kiln" "/src/kiln")
;;;;   (kiln-zero-card:sync-repos '("kiln" "cram"))   ; clone each that is missing, log a line
;;;;   kiln-zero-card:*sync-log*                      ; what SYNC-REPOS did, newest first
;;;;
;;;; THE PACK STREAMS TO THE CARD.  A fetched pack is written to the store as it arrives
;;;; (cairn's FETCH-PACK-INTO over the backend's output handle) and indexed from there a window
;;;; at a time, so a clone needs the memory of its largest object, not of its pack.  The board
;;;; has ~88 MB of semispace with ~35 MB live; a pack held whole capped clones at a few MB.
;;;;
;;;; FAT has no symlinks and no modes: a symlink is a file holding its target (git's
;;;; core.symlinks=false) and nothing is executable (core.filemode=false), which the config
;;;; written here says, so git on a host reads the checkout the same way.
;;;;
;;;; Loaded after zero-card.lisp and cairn (kiln zero installs it after a --with closure that
;;;; holds cairn).

(in-package #:kiln-zero-card)

(export '(clone-to-store store-repository sync-repos *sync-log*))

(defun %fat-join (root rel)
  "ROOT and REL as one store path, without a trailing slash."
  (string-right-trim "/" (if (or (string= rel "") (string= rel "/"))
                             root
                             (concatenate 'string root "/" rel))))

(defun %fat-mkdirs (v dir)
  "Make DIR and its parents on volume V."
  (let ((path ""))
    (dolist (part (fs-fat::%fat-split dir))
      (setq path (concatenate 'string path "/" part))
      (let ((e (fs-fat:fat32-lookup v path)))
        (cond ((null e) (fs-fat:fat32-mkdir v path))
              ((not (second e)) (error "store: ~a is a file" path)))))))

(defun %fat-parent-dirs (v path)
  (let ((slash (position #\/ path :from-end t)))
    (when (and slash (> slash 0)) (%fat-mkdirs v (subseq path 0 slash)))))

(defun %fat-put-file (v path bytes)
  (%fat-parent-dirs v path)
  (fs-fat:fat32-write-file v path (coerce bytes '(vector (unsigned-byte 8))))
  t)

(defun %fat-delete-if (v path)
  (when (fs-fat:fat32-lookup v path) (fs-fat:fat32-delete v path))
  t)

(defun %fat-walk-files (v dir prefix skip-git)
  "Paths of every file anywhere below DIR, relative to it and prefixed with PREFIX."
  (let ((out '()))
    (dolist (e (fs-fat:fat32-directory v dir) (nreverse out))
      (let ((rel (if (string= prefix "") (first e) (concatenate 'string prefix "/" (first e)))))
        (cond ((not (second e)) (push rel out))
              ((and skip-git (string-equal (first e) ".git")) nil)
              (t (setq out (revappend (%fat-walk-files v (concatenate 'string dir "/" (first e))
                                                       rel skip-git)
                                      out))))))))

(defun %bytes-string (b) (and b (map 'string #'code-char b)))
(defun %string-bytes (s) (map '(vector (unsigned-byte 8)) #'char-code s))

(defun make-store-git-backend (root)
  "A cairn FS-BACKEND keeping the git directory ROOT (e.g. \"/src/kiln/.git\") on the store."
  (let ((v (%store)))
    (flet ((full (rel) (%fat-join root rel)))
      (cairn::make-fs-backend
       :read-bytes   (lambda (rel) (fs-fat:fat32-read-file v (full rel)))
       :read-string  (lambda (rel) (%bytes-string (fs-fat:fat32-read-file v (full rel))))
       :write-bytes  (lambda (rel bytes) (%fat-put-file v (full rel) bytes))
       :write-string (lambda (rel text) (%fat-put-file v (full rel) (%string-bytes text)))
       :exists-p     (lambda (rel) (and (fs-fat:fat32-lookup v (full rel)) t))
       :delete-file  (lambda (rel) (%fat-delete-if v (full rel)))
       :dir-exists-p (lambda (rel) (let ((e (fs-fat:fat32-lookup v (full rel)))) (and e (second e) t)))
       :list-files   (lambda (rel) (loop for e in (fs-fat:fat32-directory v (full rel))
                                         unless (second e) collect (first e)))
       :walk-files   (lambda (rel) (%fat-walk-files v (full rel) (string-right-trim "/" rel) nil))
       ;; streaming: a fetched pack goes to the card as it arrives and is read back a window
       ;; at a time, never in memory whole
       :write-stream (lambda (rel next)
                       (%fat-parent-dirs v (full rel))
                       (fs-fat:fat32-write-file-stream v (full rel) next))
       :open-output  (lambda (rel)
                       (%fat-parent-dirs v (full rel))
                       (fs-fat:fat32-open-output v (full rel)))
       :write-output (lambda (h buf start end) (fs-fat:fat32-write-output h buf :start start :end end) t)
       :close-output (lambda (h abort) (fs-fat:fat32-close-output h :abort abort) t)
       :open-input   (lambda (rel) (fs-fat:fat32-open-input v (full rel)))
       :read-into    (lambda (h off buf start end) (fs-fat:fat32-read-into h off buf :start start :end end))
       :input-size   (lambda (h) (fs-fat:fat32-input-size h))
       :close-input  (lambda (h) (declare (ignore h)) t)
       :rename-file  (lambda (from to)
                       (%fat-delete-if v (full to))
                       (fs-fat:fat32-rename v (full from) (full to))
                       t)))))

(defun make-store-worktree-backend (root)
  "A cairn WT-BACKEND for the working tree ROOT on the store."
  (let ((v (%store)))
    (flet ((full (rel) (%fat-join root rel)))
      (cairn::make-wt-backend
       :read-file    (lambda (rel) (fs-fat:fat32-read-file v (full rel)))
       :write-file   (lambda (rel bytes exec) (declare (ignore exec)) (%fat-put-file v (full rel) bytes))
       :make-symlink (lambda (rel target) (%fat-put-file v (full rel) (%string-bytes target)))
       :read-symlink (lambda (rel) (%bytes-string (fs-fat:fat32-read-file v (full rel))))
       :delete-file  (lambda (rel) (%fat-delete-if v (full rel)))
       :exists-p     (lambda (rel) (and (fs-fat:fat32-lookup v (full rel)) t))
       :lstat        (lambda (rel)
                       (let ((e (fs-fat:fat32-lookup v (full rel))))
                         (when e
                           (cairn::make-wt-stat :type (if (second e) :dir :file)
                                                :mode (if (second e) #o040755 #o100644)
                                                :size (third e)))))
       :walk         (lambda (reldir) (%fat-walk-files v (full reldir) "" t))))))

(defun store-repository (dir)
  "The git repository at store directory DIR, open on the store."
  (let ((r (cairn::make-repository-on-backend (make-store-git-backend (%fat-join dir ".git"))
                                              :path dir :git-dir (%fat-join dir ".git"))))
    (setf (cairn::repo-worktree r) (make-store-worktree-backend dir))
    r))

(defun %store-start-clone (url dir refs head-target)
  "Make DIR/.git on the store with HEAD, config and REFS -- cairn's START-CLONE, on the
   store -- and return the repository."
  (let ((v (%store)))
    (when (fs-fat:fat32-lookup v dir) (error "store: ~a already exists" dir))
    (%fat-mkdirs v (%fat-join dir ".git"))
    (let ((repo (store-repository dir))
          (head-target (or head-target
                           (car (find-if (lambda (r) (search "refs/heads/" (car r))) refs))
                           "refs/heads/master")))
      (cairn::fs-write-string repo "HEAD" (format nil "ref: ~a~%" head-target))
      (cairn::fs-write-string
       repo "config"
       (format nil "[core]~%~Crepositoryformatversion = 0~%~Cbare = false~%~Csymlinks = false~%~Cfilemode = false~%[remote \"origin\"]~%~Curl = ~a~%~Cfetch = +refs/heads/*:refs/remotes/origin/*~%"
               #\Tab #\Tab #\Tab #\Tab #\Tab url #\Tab))
      (loop for (name . sha) in refs
            unless (or (string= name "HEAD") (cairn::peeled-ref-p name))
              do (cairn::fs-write-string repo name (format nil "~a~%" sha)))
      repo)))

(defun clone-to-store (url dir &key (checkout t))
  "Clone the git repository at URL (https) into store directory DIR with cairn.  The pack
   streams to the card and is indexed from there.  Returns
   (:objects N :octets N :files N :seconds S)."
  ;; index-pack's worker threads: the board's thread slots are the desk's
  (setq cairn::*index-pack-threads* 1)
  (let ((t0 (get-internal-real-time))
        (v (%store))
        (base (cairn::smart-http-base url)))
    (multiple-value-bind (refs caps head-target) (cairn::discover-refs base)
      (declare (ignore caps))
      (let ((repo (%store-start-clone url dir refs head-target)) (files 0))
        (multiple-value-bind (name count octets)
            (cairn:fetch-pack-into repo base (cairn::refs->wants refs))
          (declare (ignore name))
          (fs-fat:fat32-sync v)
          (when checkout
            (setq files (cairn:checkout repo))
            (fs-fat:fat32-sync v))
          (list :objects count :octets octets :files files
                :seconds (round (- (get-internal-real-time) t0) internal-time-units-per-second)))))))

(defvar *sync-log* '()
  "One line per repo SYNC-REPOS looked at, newest first.")

(defun sync-repos (names &key (root "/src") (org "https://github.com/modus-lisp/"))
  "kiln sync on the card, for the repos NAMES: clone each one missing from ROOT, and leave a
   present one alone (fetching into one is not here yet).  A repo that fails is logged and
   the rest still run; a half-made clone is removed so the next run tries it again.  Returns
   the lines added to *SYNC-LOG*, in order."
  (let ((lines '()))
    (dolist (name names (nreverse lines))
      (let* ((dir (concatenate 'string root "/" name))
             (line
               (if (fs-fat:fat32-lookup (%store) dir)
                   (format nil "present ~a" name)
                   (handler-case
                       (let ((r (clone-to-store (concatenate 'string org name) dir)))
                         (format nil "cloned ~a: ~d objects, ~d bytes, ~d files, ~ds" name
                                 (getf r :objects) (getf r :octets) (getf r :files) (getf r :seconds)))
                     (error (e)
                       (ignore-errors (%store-remove-tree dir))
                       (format nil "failed ~a: ~a" name (princ-to-string e)))))))
        (push line *sync-log*)
        (push line lines)))))

(defun %store-remove-tree (dir)
  "Delete DIR and everything below it from the store."
  (let ((v (%store)))
    (dolist (e (fs-fat:fat32-directory v dir))
      (let ((p (concatenate 'string dir "/" (first e))))
        (if (second e) (%store-remove-tree p) (fs-fat:fat32-delete v p))))
    (fs-fat:fat32-delete v dir)
    (fs-fat:fat32-sync v)
    t))
