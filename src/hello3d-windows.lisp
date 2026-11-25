;;;; hello3d-windows.lisp -- hello, world in 3D (hello3d.lisp) on Windows, run by modus.exe:
;;;;
;;;;   modus.exe --script vessel\src\hello3d-windows.lisp          (from the workspace: a checkout of every repo)
;;;;
;;;; The Mac's world file is kiln's boot/hello3d-world.lisp; this is its Windows counterpart, and it loads the same
;;;; vessel files on top of win-host.lisp, which answers what modus's Swift and Objective-C host answers there.
;;;; VESSEL_WORKSPACE names the workspace; else it is the directory above vessel.  HELLO3D_HEAD_DEMO set: the head
;;;; overlay sways by itself (there are no headphones to follow here).

(defvar *workspace*
  (let ((w (modus-int::%cli-getenv "VESSEL_WORKSPACE")))
    (if (and w (> (length w) 0))
        (if (member (char w (1- (length w))) '(#\/ #\\)) w (concatenate 'string w "/"))
        (namestring (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))))))

(defun workspace-file (rel) (concatenate 'string *workspace* rel))

;; seal: the UTF-8 the Swift bridge's calls carry text in; pigment: a picture dropped on the window becomes the cubes'
;; faces.  The same repos the Mac world puts on the registry.
(dolist (repo '("seal" "natrium" "cl-transport" "pigment" "scribe" "stencil" "webp-pure" "png-pure" "cram"
                "brotli-pure" "gesso" "reel"))
  (push (workspace-file (concatenate 'string repo "/")) asdf:*central-registry*))
(asdf:load-system "seal")
(asdf:load-system "pigment")

;; what the Mac world defines for the bridges: the frame hooks, and the term decoder's cursor
(defvar *kiln-frame-hooks* nil)
(defvar *term-cursor* (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
(defun decode-ptr-addr ()
  (+ (modus-int::%gc-word-of *term-cursor* (modus-int::%conv-addr #x100050A0)) 7))
(load (workspace-file "modus/net/term.lisp"))

(load (workspace-file "vessel/src/win-host.lisp"))

;; THE GENERATIONAL COLLECTOR, as kiln's desk turns it on (app.lisp, %KILN-GEN-GC-START): everything loaded so far
;; becomes an old space that collections scan through its cards and never copy, so a frame's collection copies only
;; what the frames since made.  No promotion: the loaded world would not fit in a young semispace.  VESSEL_GC=plain
;; keeps the plain collector.
(if (equal (modus-int::%cli-getenv "VESSEL_GC") "plain")
    (format t "~&hello3d: gc plain~%")
    (handler-case
        (let ((r (modus:gc-tenure :promote nil)))
          (format t "~&hello3d: gc generational, old ~D MB, young 2 x ~D MB~%"
                  (floor (- (second r) (first r)) #x100000) (floor (third r) #x100000)))
      (error (e) (format t "~&hello3d: gc tenure failed: ~A~%" e))))
(vessel-windows:open-window "Hello 3D" 1280 800)

(load (workspace-file "vessel/src/ios-swift.lisp"))
(vessel-windows:install-bridge)          ; its calls straight to win-host.lisp, no vector reached by address
(dolist (f '("mac-window" "gpu" "gamepad" "sound" "notify" "location" "motion" "link" "hello3d"))
  (load (workspace-file (concatenate 'string "vessel/src/" f ".lisp"))))

;; waveOut keeps more of its own latency than the Mac's AudioQueue: 100 ms queued ahead, not 50
(setf kiln-sound:*cushion* 4800)

(hello3d:main)
