;;;; desk-browser.lisp -- loom, the browser over weft, as a desk window on a machine with a keyboard,
;;;; a mouse and no threads (the T420's x64 desk).
;;;;
;;;; browser.lisp is the phone's: a page WORKER thread owns the page and the window only shows its
;;;; frames, a tap is a click, and the address is typed into a native prompt.  Here there are no threads
;;;; (bare x64 has none yet), a real keyboard and a mouse, so:
;;;;
;;;;   THE PAGE IS THE WINDOW'S.  loom draws straight into the window's framebuffer, and the desk's
;;;;   per-frame "anything to show?" call is the page's step: a queued navigation, then loom's pump and
;;;;   paint.  A navigation therefore holds the desk while it loads and lays out -- seconds for a big
;;;;   page.  When the machine has threads, the phone's worker split (browser.lisp) applies unchanged.
;;;;
;;;;   INPUT IS loom's OWN.  Keys and pointer events go to LOOM.GLASS:ON-KEY / ON-POINTER as they come;
;;;;   loom edits its location bar from key events, so clicking it and typing works.
;;;;
;;;;   THE START PAGE IS IN MEMORY: no Documents/ here.
;;;;
;;;; app.lisp registers KILN-BROWSER:BROWSER-APP as "Web" wherever the image has it; this file and
;;;; browser.lisp are alternatives (an image carries one).

(defpackage #:kiln-browser
  (:use #:cl)
  (:export #:browser-app))

(in-package #:kiln-browser)

;;; ---- loom's threads made into steps ------------------------------------------------------------
;;; THE SAME SWAPS AS browser.lisp's %INSTALL (the canonical copy; keep the two alike): a render is
;;; queued for the next step, a page's stylesheets and scripts are fetched in turn, and the image
;;; prefetchers that spawn threads do nothing (layout fetches each image as it lays it out).

(defvar *pending* '() "Queued navigations: (app dest on-done), oldest first.")

(defun %queue-render (app dest on-done)
  "LOOM.GLASS::%SPAWN-RENDER without a thread: queued for the window's next step."
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
          (fdefinition (find-symbol "PREFETCH-IMAGE-URLS" "LOOM"))
          (lambda (&rest args) (declare (ignore args)) '())
          (fdefinition (find-symbol "MAYBE-WARM-LAZY" "LOOM.GLASS"))
          (lambda (app) (declare (ignore app)) nil))
    (setf *installed* t)))

;;; ---- the start page ------------------------------------------------------------------------------

(defparameter +start-html+
  "<!doctype html><html><head><title>Start</title>
<style>
 body { font-family: sans-serif; margin: 18px 24px; background: #f6f4ef; color: #1d1d1f; }
 h1 { font-size: 22px; margin: 4px 0 12px; }
 p { color: #555; font-size: 14px; }
 a { display: block; padding: 8px 12px; margin: 6px 0; background: white; border-radius: 8px;
     text-decoration: none; color: #1a5fd0; box-shadow: 0 1px 3px rgba(0,0,0,.15); max-width: 560px; }
 small { color: #888; display: block; }
</style></head><body>
<h1>loom on modus</h1>
<p>A web browser in Common Lisp: weft lays out and paints, shuttle runs the JavaScript.
Click a link, or click the location at the top and type one.</p>
<a href=\"http://example.com/\">example.com<small>the smallest page there is (plain HTTP)</small></a>
<a href=\"https://example.com/\">example.com over HTTPS</a>
<a href=\"https://text.npr.org/\">text.npr.org<small>NPR, text only</small></a>
<a href=\"https://lite.cnn.com/\">lite.cnn.com<small>CNN, text only</small></a>
<a href=\"https://news.ycombinator.com/\">Hacker News</a>
<a href=\"https://en.m.wikipedia.org/wiki/Lisp_machine\">Wikipedia: Lisp machine</a>
</body></html>")

(defun %start (fb)
  "loom over the start page in FB, chrome on top (LOOM.GLASS:ATTACH-BROWSER's shape, with the page
   from memory instead of a file or URL).  The app."
  (let* ((ch-h loom.glass::+chrome-h+)
         (vw (glass:fb-width fb)) (vh (max 1 (- (glass:fb-height fb) ch-h)))
         (pg (loom:load-page +start-html+ :url "about:start" :width vw :viewport-height vh))
         (app (loom.glass::make-glass-app :fb fb :vw vw :vh vh :chrome-h ch-h)))
    (loom:render-page pg)
    (loom.glass::%init-root app pg "about:start")
    (sb-thread:with-mutex ((loom.glass::glass-app-lock app)) (loom.glass::paint app))
    app))

;;; ---- the step ------------------------------------------------------------------------------------

(defun %log (fmt &rest args)
  (format t "~&browser: ~?~%" fmt args))

(defun %step (app)
  "One frame: a queued navigation, then loom's pump.  T when the window's framebuffer changed.
   (browser.lisp's %PAGE-STEP without the worker's channel.)"
  (let ((changed nil)
        (job (find app *pending* :key #'first)))
    (when job
      (setf *pending* (remove job *pending*))
      (destructuring-bind (app dest on-done) job
        (%log "navigate ~a" dest)
        (let* ((t0 (get-internal-real-time))
               (pg (handler-case (loom.glass::load-start dest
                                                         (loom.glass::glass-app-vw app)
                                                         (loom.glass::glass-app-vh app))
                     (error (e) (%log "~a: ~a" dest e) nil))))
          (%log "loaded ~a: ~a in ~,1f s" dest (if pg "ok" "failed")
                (/ (- (get-internal-real-time) t0) internal-time-units-per-second))
          (sb-thread:with-mutex ((loom.glass::glass-app-lock app)) (funcall on-done pg))
          (setq changed t))))
    (sb-thread:with-mutex ((loom.glass::glass-app-lock app))
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

;;; ---- the window ------------------------------------------------------------------------------------

(defun browser-app (fb)
  "The desk window: loom drawing into FB, the desk's keys and pointer to it."
  (%install)
  (let ((app (%start fb)))
    (values
     (lambda (down keysym) (loom.glass::on-key app down keysym))
     (lambda (mask x y) (loom.glass::on-pointer app mask x y))
     ;; dirty-p: the page's step
     (lambda () (handler-case (%step app)
                  (error (e) (%log "step: ~a" e) nil)))
     nil
     ;; close: drop anything this window queued
     (lambda () (setf *pending* (remove app *pending* :key #'first)))
     nil nil)))

;; A long page is painted in a band around the view, not whole (browser.lisp's setting).
(setf loom:*paint-band* 3)
