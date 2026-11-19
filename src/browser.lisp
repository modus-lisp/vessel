;;;; browser.lisp -- loom, the browser over the weft web engine, as a desk window on the phone.
;;;;
;;;; loom draws a page (and its chrome: back, forward, the breadcrumb of where you have been, reload) into a
;;;; framebuffer its host owns (LOOM.GLASS:ATTACH-BROWSER), takes RFB-shaped input, and is advanced by a
;;;; pump.  The desk seats that like any window.  What a phone, and modus, change:
;;;;
;;;;   THE PAGE ON A WORKER.  loom normally renders a navigation on a thread of its own and fetches a
;;;;   page's stylesheets, scripts and images on a dozen more, handing results over under its lock.  A
;;;;   modus thread may not store its objects where another thread reaches them, so here ONE worker owns
;;;;   the whole page -- loom, weft, shuttle -- and does that work in turn, while the window only shows the
;;;;   frames it sends (see "the page worker" below).  The desk keeps running while a page loads.
;;;;
;;;;   TOUCH.  A tap is a click; a drag up or down scrolls the page (wheel steps).
;;;;
;;;;   TYPING A LOCATION.  loom edits its address bar from key events; a phone has no keyboard in the
;;;;   window, so starting an edit opens the native prompt, and what is entered is navigated to.
;;;;
;;;;   A START PAGE in Documents/web/, links to sites that are mostly text -- what a pure-Lisp engine on a
;;;;   phone does well.  JavaScript runs (shuttle), but a script that needs a generator or an async
;;;;   function needs a thread of its own in shuttle, and on modus does not run yet.

(defpackage #:kiln-browser
  (:use #:cl)
  (:export #:browser-app))

(in-package #:kiln-browser)

(defparameter +wheel-px+ 24 "Pixels of drag per wheel step.")
(defparameter +slop+ 8 "Pixels a press may move and still be a tap.")

;;; ---- threads made into steps -------------------------------------------------------------------

(defvar *pending* '() "Queued navigations: (app dest on-done), newest first.")

(defun %queue-render (app dest on-done)
  "LOOM.GLASS::%SPAWN-RENDER on the phone: queued for the window's next frame."
  (setf *pending* (append *pending* (list (list app dest on-done)))))

(defun %sequential-fetch (urls &optional (jar (symbol-value (find-symbol "*COOKIE-JAR*" "WEFT.FETCH"))))
  "LOOM::PARALLEL-FETCH in turn: abs-url -> text (NIL on failure), as the parallel one answers."
  (let ((cache (make-hash-table :test 'equal))
        (cookie-jar (find-symbol "*COOKIE-JAR*" "WEFT.FETCH")))
    (progv (list cookie-jar) (list jar)
      (dolist (u urls cache)
        (setf (gethash u cache)
              (handler-case (funcall (find-symbol "FETCH-TEXT" "WEFT.FETCH") u) (error () nil)))))))

(defvar *installed* nil)
(defun %install ()
  "Swap loom's thread-spawning functions for the step and turn-taking ones above, once."
  (unless *installed*
    (setf (fdefinition (find-symbol "%SPAWN-RENDER" "LOOM.GLASS")) #'%queue-render
          (fdefinition (find-symbol "PARALLEL-FETCH" "LOOM"))
          (lambda (urls &optional (jar nil jar-p))
            (if jar-p (%sequential-fetch urls jar) (%sequential-fetch urls)))
          ;; images: layout fetches each as it lays it out (WEFT.RENDER:FETCH-IMAGE)
          (fdefinition (find-symbol "PREFETCH-IMAGE-URLS" "LOOM"))
          (lambda (&rest args) (declare (ignore args)) '())
          (fdefinition (find-symbol "MAYBE-WARM-LAZY" "LOOM.GLASS"))
          (lambda (app) (declare (ignore app)) nil))
    (setf *installed* t)))

;;; ---- the start page -------------------------------------------------------------------------------

(defparameter +start-html+
  "<!doctype html><html><head><meta name=viewport content=\"width=device-width\">
<title>Start</title>
<style>
 body { font-family: sans-serif; margin: 14px; background: #f6f4ef; color: #1d1d1f; }
 h1 { font-size: 22px; margin: 4px 0 12px; }
 p { color: #555; font-size: 14px; }
 a { display: block; padding: 10px 12px; margin: 8px 0; background: white; border-radius: 10px;
     text-decoration: none; color: #1a5fd0; box-shadow: 0 1px 3px rgba(0,0,0,.15); }
 small { color: #888; display: block; }
</style></head><body>
<h1>loom on modus</h1>
<p>A web browser in Common Lisp: weft lays out and paints, shuttle runs the JavaScript.
Tap a link, or the location at the top to type one.</p>
<a href=\"https://example.com/\">example.com<small>the smallest page there is</small></a>
<a href=\"https://text.npr.org/\">text.npr.org<small>NPR, text only</small></a>
<a href=\"https://lite.cnn.com/\">lite.cnn.com<small>CNN, text only</small></a>
<a href=\"https://news.ycombinator.com/\">Hacker News</a>
<a href=\"https://en.m.wikipedia.org/wiki/Lisp_machine\">Wikipedia: Lisp machine</a>
</body></html>")

(defun %start-page ()
  "Documents/web/start.html, written once; its path."
  (let* ((dir (concatenate 'string (string-right-trim "/" (kiln-native:app-documents)) "/web/"))
         (path (concatenate 'string dir "start.html")))
    (ensure-directories-exist dir)
    (unless (probe-file path)
      (with-open-file (o path :direction :output :if-exists :supersede)
        (write-string +start-html+ o)))
    path))

;;; ---- the page worker ------------------------------------------------------------------------------
;;;
;;; THE PAGE LIVES ON A THREAD OF ITS OWN.  A worker owns loom over a framebuffer of its own: the DOM,
;;; the styles, the layout, the JavaScript realm and the painting.  The window keeps only pixels, and
;;; everything between the two is plain data over channels -- input and addresses in, finished frames
;;; and requests out -- so loading, scripts and layout never hold up the desk.  modus threads share no
;;; state, so the worker binds every global the page stores into (LOOM.GLASS::WITH-THREAD-STATE) and
;;; its heap is its own (48 MB on the phone, kiln's ios-world).
;;;
;;;   in   (:ptr mask x y)  (:key down keysym)  (:address text)  (:edit-done)  (:ack)  (:quit)
;;;   out  (:frame)  (:ask current)  (:died message)
;;;
;;; FRAMES GO THROUGH ONE SHARED ARRAY, not the channel.  A whole frame is 2.3 MB here (a modus pixel
;;; vector is a word a pixel) -- more than a channel's ring, and a pointless second copy if it fit.  The
;;; window makes a shared array once (MODUS:MAKE-SHARED-ARRAY); the worker copies a finished frame into
;;; it (pixels are integers, which a worker may store anywhere) and sends :FRAME, and writes the next
;;; only after the window's :ACK, so the window never reads a frame half written.

(defun %send (ch msg) (modus:channel-send ch msg))

(defvar *t0* (get-internal-real-time))
(defun %log (fmt &rest args)
  "A line in modus.log, stamped with ms since load, written through at once."
  (format t "~&browser [~d ms]: ~?~%"
          (round (* 1000 (- (get-internal-real-time) *t0*)) internal-time-units-per-second) fmt args)
  (finish-output))
(defun %poll (ch) (modus:channel-try-receive ch))

(defun %page-step (app asking out)
  "One frame of the worker: a queued navigation, a request for the address, loom's pump.  T when the
   page's framebuffer changed.  ASKING is a cons whose car is true while the window's prompt is up."
  ;; a queued navigation (loom's %SPAWN-RENDER, made a step): render it here
  (let ((job (find app *pending* :key #'first)))
    (when job
      (setf *pending* (remove job *pending*))
      (destructuring-bind (app dest on-done) job
        (%log "navigate ~a" dest)
        (let ((pg (handler-case (loom.glass::load-start dest
                                                        (loom.glass::glass-app-vw app)
                                                        (loom.glass::glass-app-vh app))
                    (error (e) (%log "~a: ~a" dest e) nil))))
          (%log "loaded ~a: ~a" dest (if pg "ok" "failed"))
          (sb-thread:with-mutex ((%lock app)) (funcall on-done pg))
          (%log "shown ~a" dest)))))
  ;; the location bar was opened for editing: the window asks natively
  (when (and (loom.glass::glass-app-editing app) (not (car asking)))
    (setf (car asking) t)
    (%send out (list :ask (copy-seq (loom.glass::glass-app-edit-buf app)))))
  ;; loom's pump, once
  (let ((changed nil))
    (sb-thread:with-mutex ((%lock app))
      (let ((pg (loom.glass::glass-app-page app)))
        (when (and pg (loom::pump pg))
          (setf (loom.glass::glass-app-dirty app) t)))
      (when (loom.glass::any-loading-p app)
        (setf (loom.glass::glass-app-dirty app) t))
      (when (loom.glass::glass-app-dirty app)
        (loom.glass::paint app)
        (setf (loom.glass::glass-app-dirty app) nil)
        (setq changed t)))
    changed))

(defun %page-handle (app msg asking)
  "Apply one message from the window.  :QUIT answers :QUIT."
  (case (first msg)
    (:ptr (destructuring-bind (mask x y) (rest msg)
            (when (eql mask 1) (%log "tap ~d,~d" x y))
            (loom.glass::on-pointer app mask x y)))
    (:key (destructuring-bind (down keysym) (rest msg) (loom.glass::on-key app down keysym)))
    (:address (sb-thread:with-mutex ((%lock app))
                (setf (loom.glass::glass-app-edit-buf app) (copy-seq (second msg)))
                (loom.glass::edit-key app #xff0d)))
    (:edit-done (sb-thread:with-mutex ((%lock app))
                  (setf (loom.glass::glass-app-editing app) nil))
                (setf (car asking) nil))
    (:quit :quit)))

(defun %page-worker (start w h shared in out reply)
  "The worker's whole life: loom over START in a W x H framebuffer, stepped until :QUIT; frames are
   copied into SHARED."
  (lambda ()
    (handler-case
        (loom.glass::with-thread-state ()
          (let* ((*pending* '())
                 (loom:*progress* (lambda (phase &optional detail) (%log "  ~(~a~) ~@[~a~]" phase detail)))
                 (scribe::*face-opener* (%face-opener out reply))
                 (fb (glass:make-framebuffer w h))
                 (app (loom.glass::attach-browser start fb))
                 (asking (list nil))
                 (owed t)                ; a frame the window has not been sent
                 (acked t))              ; the window has taken the last one
            (loop
              (let ((msg (%poll in)) (quit nil))
                (loop while msg
                      do (if (eq (first msg) :ack)
                             (setq acked t)
                             (when (eq (%page-handle app msg asking) :quit) (setq quit t)))
                         (setq msg (%poll in)))
                (when quit (return))
                (when (%page-step app asking out) (setq owed t))
                (if (and owed acked)
                    (progn (replace shared (glass:fb-pixels fb))
                           (%send out '(:frame))
                           (setq owed nil acked nil))
                    (sleep 0.015))))))
      (error (e) (%send out (list :died (princ-to-string e)))))
    (%log "page worker ended")))

;;; ---- the window ------------------------------------------------------------------------------------

(defun %lock (app) (loom.glass::glass-app-lock app))

;;; ---- fallback faces, opened on main when a page needs them ------------------------------------------

;;; scribe draws characters its primary face lacks (Chinese, Arabic, emoji ...) from fallback faces, which kiln
;;; puts in the app bundle's fonts/ as files.  A page worker must not read one itself: a face is megabytes (the
;;; Chinese one 10 MB), and Wikipedia's language list alone pulled 17 MB of them into a worker's heap.  So the
;;; worker asks the window, which opens the face on MAIN into a slot here, and the worker reads main's copy.  A
;;; face is opened only when some page needs it, once, and every page worker shares it.

(defparameter *face-names*
  (remove-duplicates (append (mapcar #'third scribe::*fallback-blocks*) scribe::*fallback-probe*)
                     :test #'string= :from-end t)
  "Every fallback face scribe can ask for; a face's slot is its position here.")
(defvar *face-slots* (make-array (length *face-names*) :initial-element nil)
  "Per face: NIL (not asked for yet), the open face, or :FAILED.  Written only on main.")

(defun %bundle-fonts ()
  "Point scribe at the bundle's fonts/ for the faces main opens (main only)."
  (let ((dir (and (fboundp 'cl-user::%kiln-bundle-dir) (funcall 'cl-user::%kiln-bundle-dir))))
    (when dir
      (setf scribe::*fonts-dir* (pathname (concatenate 'string (string-right-trim "/" dir) "/fonts/"))))))

(defun %open-face-on-main (i)
  "Main's half: open face I into its slot, unless it is already there."
  (unless (svref *face-slots* i)
    (setf (svref *face-slots* i)
          (or (ignore-errors (scribe::%fallback-open (nth i *face-names*))) :failed))))

(defun %face-opener (out reply)
  "The worker's half, as SCRIBE:*FACE-OPENER*: ask the window for the face and wait for it."
  (lambda (fname)
    (let ((i (position fname *face-names* :test #'string=)))
      (when i
        (unless (svref *face-slots* i)
          (%send out (list :open-face i))
          (modus:channel-receive reply))
        (let ((v (svref *face-slots* i))) (if (eq v :failed) nil v))))))

(defun browser-app (fb)
  "The desk window: pixels from the page worker, and touches, keys and addresses to it."
  (%install)
  (%bundle-fonts)
  (let* ((in (modus:make-channel :name "browser-in"))
         (reply (modus:make-channel :name "browser-face" :capacity 256))
         (out (modus:make-channel :name "browser-out"))
         (shared (modus:make-shared-array (* (glass:fb-width fb) (glass:fb-height fb))
                                          :element-type '(unsigned-byte 32)))
         (press nil)                     ; (x0 y0 dragged last-y) while a finger is down
         (alive t))
    (sb-thread:make-thread (%page-worker (%start-page) (glass:fb-width fb) (glass:fb-height fb) shared in out reply)
                           :name "browser page")
    (flet ((ptr (mask x y) (%send in (list :ptr mask x y))))
      (values
       (lambda (down keysym) (%send in (list :key down keysym)))
       (lambda (mask x y)
         (cond
           ((logtest mask 1)
            (if (null press)
                (setf press (list x y nil y))
                (destructuring-bind (x0 y0 dragged ly) press
                  (when (or dragged (> (max (abs (- x x0)) (abs (- y y0))) +slop+))
                    (setf (third press) t)
                    (loop while (>= (- ly y) +wheel-px+) do (ptr 16 x y) (ptr 0 x y) (decf ly +wheel-px+))
                    (loop while (>= (- y ly) +wheel-px+) do (ptr 8 x y) (ptr 0 x y) (incf ly +wheel-px+))
                    (setf (fourth press) ly)))))
           (press
            (destructuring-bind (x0 y0 dragged ly) press
              (declare (ignore ly))
              (setf press nil)
              (unless dragged (ptr 1 x0 y0) (ptr 0 x0 y0))))))
       ;; dirty-p: take what the worker sent
       (lambda ()
         (let ((changed nil))
           (loop for msg = (%poll out) while msg
                 do (case (first msg)
                      (:frame (replace (glass:fb-pixels fb) shared)
                              (%send in '(:ack))
                              (setq changed t))
                      (:ask (kiln-operandi:ask-text
                             :title "Open" :label "Address" :ok "Go" :current (second msg)
                             :done (lambda (text) (%send in (list :address text))))
                            (%send in '(:edit-done)))
                      (:open-face (%open-face-on-main (second msg))
                                  (%send reply '(:face)))
                      (:died (format t "~&browser: the page worker stopped: ~a~%" (second msg))
                             (setq alive nil))))
           changed))
       nil
       ;; close: end the worker, which frees its thread and heap
       (lambda () (when alive (%send in '(:quit)) (setq alive nil)))
       nil nil))))

;; At load, in the world build: the snapshot carries the swapped functions, so nothing is redefined on the phone.
(%install)

;; PAINT A BAND, NOT THE WHOLE PAGE: three screens around what is in view, repainted from the same layout as
;; a scroll leaves it.  loom's whole-length canvas is tens of megabytes on a long article (402 x 12821 x 3
;; for Wikipedia's Lisp machine page) -- most of a page worker's heap.
(setf loom:*paint-band* 3)
