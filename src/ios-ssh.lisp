;;;; ios-ssh.lisp -- the SSH that the phone's Terminal uses: conch for the connection, and the
;;;; device's identity and known hosts kept in the system keychain (modus-ui.m's pseudo-syscalls
;;;; 1030-1032), not in a file.  The Terminal finds this package by the three names its ssh needs,
;;;; as it finds the board's (zero-ssh.lisp), and it is loaded into the phone's world (kiln's
;;;; boot/ios-world.lisp).

(defpackage #:kiln-ios-ssh
  (:use #:cl)
  (:export #:ssh-install #:ssh-pubkey))

(in-package #:kiln-ios-ssh)

(defvar *identity* nil "(SEED-32 PUBLIC-32), read from the keychain or made on first use.")

;;; The keychain and the random bytes.  A buffer's address is taken the way app.lisp takes a
;;; blit's: the data is passed to the shim at once, and no allocation comes between taking an
;;; address and the call.

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun %cstring (name)
  (let ((a (make-array (1+ (length name)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i (length name) a)
      (setf (aref a i) (char-code (char name i))))))

(defun %keychain-get (name)
  "The bytes kept under NAME (at most 512), or NIL.  The keychain answers a length, 0 for none."
  (let* ((nm (%cstring name))
         (buf (make-array 512 :element-type '(unsigned-byte 8)))
         (n (cl-user::%kiln-sys 1030 (%addr nm) (%addr buf) 512)))
    (and (integerp n) (> n 0) (subseq buf 0 n))))

(defun %keychain-put (name bytes)
  "Keep BYTES under NAME, replacing what was there.  True if the keychain took them."
  (let* ((nm (%cstring name))
         (data (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (r (cl-user::%kiln-sys 1031 (%addr nm) (%addr data) (length data))))
    (eql r 1)))

(defun %random-bytes (n)
  (let* ((buf (make-array n :element-type '(unsigned-byte 8)))
         (r (cl-user::%kiln-sys 1032 (%addr buf) n 0)))
    (unless (eql r 1) (error "kiln: no random bytes for the SSH key"))
    buf))

;;; ---- the identity (conch's connect takes it as (SEED PUBLIC)) ----

(defun %identity ()
  (or *identity*
      (setf *identity*
            (let ((seed (%keychain-get "ssh-seed")))
              (unless (and seed (= (length seed) 32))
                (setq seed (%random-bytes 32))
                (unless (%keychain-put "ssh-seed" seed)
                  (error "kiln: the keychain refused the SSH key")))
              (list seed (funcall (find-symbol "ED25519-PUBLIC-KEY" "NATRIUM") seed))))))

;;; ---- the host keys: trust on first use, kept in the keychain ----

(defun %verifier (host)
  (lambda (blob)
    (let* ((key (concatenate 'string "known:" host))
           (seen (%keychain-get key)))
      (cond ((null seen)
             (unless (%keychain-put key blob)
               (error "kiln: the keychain refused ~a's host key" host)))
            ((not (equalp seen blob))
             (error "kiln: ~a's host key CHANGED" host))))))

;;; ---- what the Terminal calls ----

(defun ssh-install () t)                ; the phone has sockets already: nothing to install

(defun %ssh-string (bytes)
  "An SSH wire string: a 4-byte big-endian length, then the bytes."
  (let* ((n (length bytes))
         (out (make-array (+ 4 n) :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (ldb (byte 8 24) n) (aref out 1) (ldb (byte 8 16) n)
          (aref out 2) (ldb (byte 8 8) n) (aref out 3) (ldb (byte 8 0) n))
    (replace out bytes :start1 4)
    out))

(defun ssh-pubkey ()
  "The line for a server's authorized_keys: this device's public key, in OpenSSH form."
  (let* ((pub (second (%identity)))
         (blob (concatenate '(simple-array (unsigned-byte 8) (*))
                            (%ssh-string (map '(simple-array (unsigned-byte 8) (*)) #'char-code "ssh-ed25519"))
                            (%ssh-string pub))))
    (format nil "ssh-ed25519 ~a modus-phone"
            (funcall (find-symbol "BASE64-ENCODE" "CONCH") blob))))
