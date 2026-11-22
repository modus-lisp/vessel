;;;; dict.lisp DICT INPUT OUT -- INPUT as a raw LZMA1 stream (lc 2 lp 1 pb 2, no
;;;; end marker) against the preset dictionary DICT, by cram's encoder: what
;;;; run.sh feeds the harness to check mac68k-lzma.inc's dictionary reads.
;;;; cram is $CRAM, else the workspace's cram/ beside vessel.
(let* ((env (sb-ext:posix-getenv "CRAM"))
       (dir (if (and env (plusp (length env)))
                (pathname (if (char= (char env (1- (length env))) #\/) env (concatenate 'string env "/")))
                (merge-pathnames "../../../cram/" *load-truename*))))
  (load (merge-pathnames "src/packages.lisp" dir))
  (dolist (f '("deflate" "lzma"))
    (let ((fasl (compile-file (merge-pathnames (format nil "src/~A.lisp" f) dir)
                              :output-file (format nil "/tmp/unlzma-dict-~A-~D.fasl" f
                                                   (random 1000000000 (make-random-state t))))))
      (unwind-protect (load fasl) (delete-file fasl)))))
(flet ((read-bytes (path)
         (with-open-file (in path :element-type '(unsigned-byte 8))
           (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
             (read-sequence v in) v))))
  (destructuring-bind (dict input out) (rest sb-ext:*posix-argv*)
    (with-open-file (o out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence (funcall (find-symbol "LZMA-COMPRESS" "CRAM") (read-bytes input)
                               :dict (read-bytes dict) :lc 2 :lp 1 :pb 2 :end-marker nil)
                      o))))
