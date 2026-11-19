;;;; operandi.lisp -- the operandi agent as a desk window: operandi-gui's chat, drawn for a phone, with
;;;; the PHONE's own text input.
;;;;
;;;; The conversation is operandi-gui's ROWS (the same messages the browser's warp table shows), drawn
;;;; here as a phone draws a chat: bubbles, yours on the right and the agent's on the left, words wrapped
;;;; to the window, the agent's working notes small and grey between them, newest at the bottom.  A drag
;;;; scrolls back; at the bottom the view follows what arrives.
;;;;
;;;; What a framebuffer cannot do is take text, so the phone's input does that: a "Message..." bar along
;;;; the window's bottom, whose tap is a native alert with a field and Send, and what is sent is
;;;; OPERANDI-GUI:SAY -- the same entry the browser's <input> and the gateway use.  ASK-TEXT is that alert
;;;; on its own, for anything else that has to ask (a key: SECRET makes the field secure).
;;;;
;;;; THREADS.  Everything here runs on the desk's thread: the alert's answer arrives through
;;;; ios-swift.lisp's event pump, a frame hook.  operandi-gui's worker is a thread of its own and shares
;;;; nothing with this one; reading ROWS applies what it has sent (operandi-gui's PUMP), so the window
;;;; reads them every frame, and the Eval tool's forms run here, inside that read.

(defpackage #:kiln-operandi
  (:use #:cl)
  (:export #:operandi-app #:ask-text #:compose))

(in-package #:kiln-operandi)

(defparameter +bar-h+ 48 "The message bar's height, in desk pixels: a touch target and a margin.")
(defparameter +bg+ #x0e1117)
(defparameter +bar-bg+ #x161b24)
(defparameter +field-bg+ #x232a36)
(defparameter +hint+ #x8a94a6)
(defparameter +you-bg+ #x2f6feb)
(defparameter +you-ink+ #xffffff)
(defparameter +bot-bg+ #x1f2630)
(defparameter +bot-ink+ #xe6e9ef)
(defparameter +err-bg+ #x4a1f24)
(defparameter +err-ink+ #xffd7d9)
(defparameter +think-ink+ #x8a94a6)

(defparameter +size+ 16 "Message text, px.")
(defparameter +lh+ 21 "Its line height.")
(defparameter +think-size+ 13)
(defparameter +think-lh+ 17)
(defparameter +margin+ 10 "Between the window's edge and a bubble.")
(defparameter +pad-x+ 12 "Inside a bubble, left and right.")
(defparameter +pad-y+ 8 "Inside a bubble, top and bottom.")
(defparameter +gap+ 8 "Between two messages.")
(defparameter +radius+ 12)
(defparameter +tap-slop+ 8 "A press that moves less than this is a tap, not a drag.")

(defvar *started* nil)

(defun %data-dirs ()
  "Where the agent keeps its chats, on THIS phone.  operandi-gui computes them from the home directory
   when it is LOADED -- on the Mac, so the snapshot names the Mac's -- and an iOS app's home (its container)
   is not writable in any case.  Documents/operandi/, which the Files app shows."
  (let* ((docs (kiln-native:app-documents))
         (root (concatenate 'string (string-right-trim "/" docs) "/operandi/")))
    (setf (symbol-value (find-symbol "*SESSIONS-DIR*" "OPERANDI.SESSION")) (concatenate 'string root "chats/")
          (symbol-value (find-symbol "*FILES-ROOT*" "OPERANDI-GUI")) (pathname (concatenate 'string root "files/"))
          (symbol-value (find-symbol "*TRASH-ROOT*" "OPERANDI-GUI")) (pathname (concatenate 'string root "trash/"))
          (symbol-value (find-symbol "*OFFLOAD-DIR*" "OPERANDI.ENGINE")) (concatenate 'string root "offload/"))
    ;; where the agent's relative paths land (Imagine's generated/...): the app's container is not writable
    (let ((work (concatenate 'string root "work/")))
      (ensure-directories-exist work)
      (setf *default-pathname-defaults* (pathname work)))
    root))

(defun %sync-images ()
  "Imagine, the agent's picture tool, whenever this phone has a PPQ key: PPQ makes the pictures and the
   account pays (about five cents each).  Without one, no Imagine."
  (let ((key (recall-key :ppq)))
    (setf operandi.tools:*images* (and key :ppq)
          operandi.tools:*image-token* key
          ;; the model sees what it made, small: iOS shrinks it to a JPEG (%NATIVE-SHRINKER), and only
          ;; the last two stay in the conversation -- each is base64 in every request the worker builds
          operandi.tools:*tool-images-send* t
          operandi.engine::*tool-image-side* 512
          operandi.engine::*tool-images-kept* 2)))

;;; THE MODEL'S COPY OF A PICTURE.  The engine shows the model each picture a tool made, shrunk -- in
;;; Lisp, by default, which decodes the whole picture on the agent's worker thread: more heap than it
;;; has.  Here iOS (ImageIO) makes a small JPEG and the worker reads only that.  The worker cannot call
;;; into iOS itself, so it asks the window's thread, as it does for Eval (operandi-gui's host calls).

(defun %model-copy (path side)
  "On the window's thread: a JPEG of PATH no larger than SIDE pixels, made once; its path, or NIL."
  (let* ((dir (concatenate 'string *root* "work/.model/"))
         (out (format nil "~a~36r-~d.jpg" dir (sxhash (namestring path)) side)))
    (ensure-directories-exist dir)
    (if (or (probe-file out) (kiln-native:app-image-jpeg (namestring path) side out :quality 70))
        out
        nil)))

(defun %install-native-shrinker ()
  (setf (gethash "%model-copy" operandi-gui::*host-tool-impls*)
        (lambda (args) (destructuring-bind (path side) args (%model-copy path side))))
  (setf operandi.llm:*image-shrinker*
        (lambda (path side)
          (let ((out (if operandi-gui::*worker-out*      ; on the worker: ask the window's thread
                         (operandi-gui::host-tool-call "%model-copy" (list (namestring path) side))
                         (%model-copy path side))))
            (when (and (stringp out) (not (search "TOOL ERROR" out)) (ignore-errors (probe-file out)))
              (with-open-file (in out :element-type '(unsigned-byte 8))
                (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                  (read-sequence v in)
                  (values v "image/jpeg"))))))))

(defvar *root* nil "Documents/operandi/, once %DATA-DIRS has run.")

(defun ensure-started ()
  "The agent's worker, once.  At first open, not at load: a snapshot carries no threads (ios-world.lisp).
   Then the inference settings: the saved ones applied, or -- the first time -- the panel."
  (unless *started*
    (setf *root* (%data-dirs)
          ;; pictures are drawn from their files, natively: no Lisp-decoded preview in each row
          operandi-gui:*row-previews* nil
          ;; an iPhone runs no programs: no Bash for the model to try
          operandi.tools:*processes* nil)
    (setf operandi-gui:*preflight-fn* #'%preflight-answer)
    (%install-native-shrinker)
    ;; a test's stand-in for PPQ's API (the Simulator, against a local server): OPERANDI_PPQ_API
    (let ((u (uiop:getenv "OPERANDI_PPQ_API")))
      (when (and u (plusp (length u))) (setf operandi.payments::*ppq-api* u operandi.ppq:*ppq-base* u)))
    (operandi-gui:start-agent)
    (format t "~&kiln: thread heaps: ~D of ~D MB~%"
            modus-int::*ha-nregions* (floor modus-int::*ha-rsize* (* 1024 1024)))
    (setf *started* t)
    (%sync-images)
    (let ((saved (load-settings)))
      (format t "~&operandi: settings ~:[none saved~;~:*~s~] (~a)~%" saved (%settings-path))
      (cond (saved (apply-settings saved :quiet t))
            ;; nothing saved, but this phone has a PPQ account: use it rather than ask again
            ((recall-key :ppq)
             (apply-settings (list :backend :ppq :model (third (%backend :ppq))) :quiet t))
            (t (open-settings :first-run t))))
    ;; the chat you were in
    (let ((id (%latest-session-id)))
      (when id (ignore-errors (operandi-gui:open-session! id))))))

(defun %latest-session-id ()
  "The chat saved most recently, by its file's date (no chat is read to find it), or NIL."
  (let* ((files (ignore-errors (directory (merge-pathnames "*.json" (symbol-value (find-symbol "*SESSIONS-DIR*" "OPERANDI.SESSION"))))))
         (newest (first (sort (copy-list files) #'> :key (lambda (f) (or (ignore-errors (file-write-date f)) 0))))))
    (and newest (pathname-name newest))))

(defun open-chats ()
  "The bar's Chats: a new chat, or one of the recent ones."
  (if (operandi-gui:busy-p)
      (%alert "A turn is running" "Switch chats when it has finished." (cons "OK" nil))
      (let* ((chats (let ((all (ignore-errors (funcall (find-symbol "LIST-SESSIONS" "OPERANDI.SESSION")))))
                      (subseq all 0 (min 20 (length all)))))
             (current (operandi-gui:current-session-id))
             (a (kiln-native:app-alert "Chats" nil :sheet t))
             (actions '()))
        (kiln-native:app-alert-action a "New Chat")
        (push (lambda () (operandi-gui:new-session!)) actions)
        (dolist (c chats)
          (destructuring-bind (id turns first &rest _) c
            (declare (ignore _))
            (let* ((t1 (substitute #\Space #\Newline (string-trim " " (or first ""))))
                   (title (cond ((zerop (length t1)) "(empty chat)")
                                ((> (length t1) 40) (concatenate 'string (subseq t1 0 40) "..."))
                                (t t1))))
              (kiln-native:app-alert-action
               a (format nil "~:[~;* ~]~a  (~d)" (equal id current) title (or turns 0)))
              (push (let ((id id)) (lambda () (operandi-gui:open-session! id))) actions))))
        (setf actions (nreverse actions))
        (kiln-native:app-alert-action a "Cancel" :style :cancel)
        (kiln-native:on-event :alert a (lambda (v h) (declare (ignore h))
                                         (let ((fn (and (consp v) (nth (first v) actions))))
                                           (when fn (funcall fn)))))
        (kiln-native:app-alert-show a))))

(defun ask-text (&key label current secret done (title "operandi") (ok "OK"))
  "A native alert with one field -- secure for a SECRET, and then never pre-filled -- and DONE called
   with the text when OK is tapped and it is not empty.  Cancel calls nothing."
  (let ((a (kiln-native:app-alert title (or label ""))))
    (kiln-native:app-alert-field a (or label "") :text (if secret "" (or current "")) :secure secret)
    (kiln-native:app-alert-action a "Cancel" :style :cancel)          ; index 0
    (kiln-native:app-alert-action a ok)                               ; index 1
    (kiln-native:on-event :alert a
                          (lambda (v handle)
                            (declare (ignore handle))
                            (when (and (consp v) (eql (first v) 1))
                              (let ((text (second v)))
                                (when (and (stringp text) (plusp (length text)) done)
                                  (funcall done text))))))
    (kiln-native:app-alert-show a)))

(defun compose ()
  "The message bar's tap: ask, and say what comes back.  /settings opens the inference settings."
  (ask-text :label "Message" :ok "Send"
            :done (lambda (text)
                    (if (string-equal (string-trim " " text) "/settings")
                        (open-settings)
                        (operandi-gui:say text)))))

;;; ------------------------------------------------------------------ text

(defparameter +stand-ins+
  '((#\⚙ . "•") (#\↪ . "→") (#\✗ . "×") (#\… . "...") (#\— . "-") (#\– . "-")
    (#\“ . "\"") (#\” . "\"") (#\‘ . "'") (#\’ . "'") (#\→ . "->") (#\• . "-") (#\× . "x"))
  "What to draw for a character the font may not have, tried in order until one it has: the agent's
   status line uses ⚙ and ↪, which Liberation Sans lacks and drew as boxes.")

(defun %has-glyph (font ch)
  (let ((g (ignore-errors (scribe:font-glyph-index font (char-code ch))))) (and g (plusp g))))

(defun %drawable (s font)
  "S with every character the font has no glyph for replaced by a stand-in it does have, or dropped."
  (with-output-to-string (o)
    (loop for ch across s
          do (cond ((or (char= ch #\Space) (%has-glyph font ch)) (write-char ch o))
                   (t (let ((alt ch))
                        ;; follow the chain: ⚙ -> • -> -
                        (loop repeat 3
                              do (let ((sub (cdr (assoc alt +stand-ins+))))
                                   (cond ((null sub) (setf alt nil) (return))
                                         ((every (lambda (c) (%has-glyph font c)) sub)
                                          (write-string sub o) (setf alt nil) (return))
                                         ((= (length sub) 1) (setf alt (char sub 0)))
                                         (t (setf alt nil) (return)))))))))))

(defun %plain (text)
  "The agent's markdown as text a phone can show in one face: the emphasis and code marks dropped, a
   fence line dropped, tabs as spaces.  Returns its lines."
  (let ((lines '()))
    (with-input-from-string (in (substitute #\Space #\Tab (remove #\Return (or text ""))))
      (loop for line = (read-line in nil) while line
            unless (and (>= (length line) 3) (string= "```" (string-left-trim " " line) :end2 3))
              do (push (let ((s line))
                         (loop for mark in '("**" "__" "`")
                               do (loop for p = (search mark s) while p
                                        do (setf s (concatenate 'string (subseq s 0 p)
                                                                (subseq s (+ p (length mark)))))))
                         (if (and (>= (length s) 2) (string= "# " s :end2 2)) (subseq s 2)
                             (string-left-trim "#" s)))
                       lines)))
    ;; no blank lines at the ends
    (let ((ls (nreverse lines)))
      (loop while (and ls (string= "" (string-trim " " (first ls)))) do (pop ls))
      (setf ls (reverse ls))
      (loop while (and ls (string= "" (string-trim " " (first ls)))) do (pop ls))
      (or (nreverse ls) (list "")))))

(defun %wrap (line width size font)
  "LINE broken into rows no wider than WIDTH: at spaces, and inside a word only when the word alone is
   wider than a row."
  (if (string= line "")
      (list "")
      (let ((rows '()) (row ""))
        (flet ((fits (s) (<= (glass:text-width s :size size :font font) width)))
          (dolist (word (let ((acc '()) (start 0))
                          (loop for k from 0 below (length line)
                                when (char= (char line k) #\Space)
                                  do (push (subseq line start (1+ k)) acc) (setf start (1+ k)))
                          (push (subseq line start) acc)
                          (nreverse (remove "" acc :test #'string=))))
            (cond ((fits (concatenate 'string row (string-right-trim " " word)))
                   (setf row (concatenate 'string row word)))
                  (t
                   (when (plusp (length row)) (push (string-right-trim " " row) rows))
                   (setf row "")
                   (loop for ch across word
                         do (if (fits (concatenate 'string row (string ch)))
                                (setf row (concatenate 'string row (string ch)))
                                (progn (push row rows) (setf row (string ch))))))))
          (push (string-right-trim " " row) rows)
          (nreverse rows)))))

(defun %file-caption (text)
  "A file row's text is JSON (operandi-gui's FILE-ROW-TEXT): its name and note, for a caption."
  (let ((h (ignore-errors (json-simple:parse text))))
    (if (hash-table-p h)
        (format nil "File: ~a~@[~%~a~]" (gethash "name" h "?") (gethash "note" h))
        text)))

;;; ------------------------------------------------------------------ layout

(defstruct blk
  side      ; :right (yours) :left (the agent's) :none (its working notes)
  lines     ; the wrapped rows
  size lh ink bg
  w h       ; the bubble's size, padding included (for :NONE, the text's)
  pic       ; a picture's thumbnail (a framebuffer) above the lines, or NIL
  path      ; the file a tap opens, or NIL
  id kind   ; the message's id and kind
  text)     ; what Copy copies: the message as it came

(defparameter +think-rows+ 4 "How many lines of the agent's working notes show until they are tapped open.")
(defvar *expanded* (make-hash-table) "Message ids whose working notes are tapped open.")

;;; ------------------------------------------------------------------ pictures
;;;
;;; A file row that is a picture (what Imagine made, or Read opened) is drawn as the picture: a thumbnail
;;; in the bubble, the whole thing in Quick Look on a tap.  Decoding a 1024-pixel PNG in Lisp takes long
;;; enough to stop the window, so it is decoded and shrunk in the BACKGROUND and only the thumbnail comes
;;; back.  *THUMB-EPOCH* moves when one arrives, so the window lays itself out again.

(defparameter +pic-max-w+ 280)
(defparameter +pic-max-h+ 320)
(defvar *thumbs* (make-hash-table :test 'equal) "path -> :pending, :failed, or a framebuffer.")
(defvar *thumb-epoch* 0)

(defun %picture-name-p (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (member (subseq name (1+ dot)) '("png" "jpg" "jpeg" "webp" "gif") :test #'string-equal))))

(defun %thumb-pixels (path maxw maxh)
  "On a background thread: the picture at PATH, area-averaged down to fit MAXW x MAXH, as
   (W H PIXELS), PIXELS #xRRGGBB words with any alpha over the bubble's colour; NIL if it does not decode."
  (let* ((bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence v in) v)))
         (img (ignore-errors (pigment:decode-image-bytes bytes))))
    (when img
      (let* ((sw (pigment:img-w img)) (sh (pigment:img-h img)) (rgba (pigment:img-rgba img))
             (scale (min 1 (/ maxw sw) (/ maxh sh)))
             (tw (max 1 (floor (* sw scale)))) (th (max 1 (floor (* sh scale))))
             (out (make-array (* tw th) :element-type '(unsigned-byte 32)))
             (bg-r (ldb (byte 8 16) +bot-bg+)) (bg-g (ldb (byte 8 8) +bot-bg+)) (bg-b (ldb (byte 8 0) +bot-bg+)))
        (dotimes (ty th)
          (let ((y0 (floor (* ty sh) th)) (y1 (max (1+ (floor (* ty sh) th)) (floor (* (1+ ty) sh) th))))
            (dotimes (tx tw)
              (let ((x0 (floor (* tx sw) tw)) (x1 (max (1+ (floor (* tx sw) tw)) (floor (* (1+ tx) sw) tw)))
                    (r 0) (g 0) (b 0) (n 0))
                (loop for y from y0 below (min y1 sh)
                      do (loop for x from x0 below (min x1 sw)
                               do (let* ((i (* 4 (+ x (* y sw)))) (a (aref rgba (+ i 3))) (ia (- 255 a)))
                                    (incf r (floor (+ (* a (aref rgba i)) (* ia bg-r)) 255))
                                    (incf g (floor (+ (* a (aref rgba (+ i 1))) (* ia bg-g)) 255))
                                    (incf b (floor (+ (* a (aref rgba (+ i 2))) (* ia bg-b)) 255))
                                    (incf n))))
                (setf n (max n 1))
                (setf (aref out (+ tx (* ty tw)))
                      (logior (ash (floor r n) 16) (ash (floor g n) 8) (floor b n)))))))
        (list tw th out)))))

(defun %native-thumb (path)
  "PATH's thumbnail from iOS (ImageIO), as a framebuffer, or :FAILED; NIL where there is no native one."
  (let ((f (find-symbol "APP-THUMBNAIL" "KILN-NATIVE")))
    (when (and f (fboundp f))
      (multiple-value-bind (w h px)
          (ignore-errors (funcall f path (max +pic-max-w+ +pic-max-h+) :background +bot-bg+))
        (if w
            (let ((fb (glass:make-framebuffer w h)))
              (replace (glass:fb-pixels fb) px)
              fb)
            :failed)))))

(defun %thumb (path)
  "PATH's thumbnail framebuffer, or :PENDING (being made) or :FAILED.  On the phone iOS makes it, at
   once: decoding a 1024-pixel picture in Lisp takes more heap than a worker thread has.  Elsewhere
   (a test on the Mac) pigment decodes it in the background."
  (let ((have (or (gethash path *thumbs*)
                  (let ((n (%native-thumb path)))
                    (when n (setf (gethash path *thumbs*) n))))))
    (or have
        (progn
          (setf (gethash path *thumbs*) :pending)
          (background (lambda () (%thumb-pixels path +pic-max-w+ +pic-max-h+))
                      (lambda (r)
                        (setf (gethash path *thumbs*)
                              (let ((v (and (eq (first r) :ok) (second r))))
                                (if v
                                    (destructuring-bind (w h px) v
                                      (let ((fb (glass:make-framebuffer w h)))
                                        (replace (glass:fb-pixels fb) px)
                                        fb))
                                    :failed)))
                        (incf *thumb-epoch*)))
          :pending))))

(defun %file-picture (text)
  "For a file row's JSON TEXT: (values PATH NOTE) when it is a picture this phone has, else NIL."
  (let ((h (ignore-errors (json-simple:parse text))))
    (when (hash-table-p h)
      (let* ((id (gethash "id" h)) (name (gethash "name" h "")) (href (gethash "href" h ""))
             ;; href is /file/SID/ID
             (parts (remove "" (uiop:split-string href :separator "/") :test #'string=))
             (sid (and (= (length parts) 3) (second parts))))
        (when (and id sid (stringp name) (%picture-name-p name))
          (let ((path (ignore-errors (operandi-gui:stored-file id sid))))
            (when path (values (namestring path) (gethash "note" h)))))))))

(defun %layout (msg width font)
  "One message as a block for a window WIDTH wide."
  (when (eq (operandi-gui:msg-kind msg) :file)
    (multiple-value-bind (path note) (%file-picture (operandi-gui:msg-text msg))
      (when path
        (return-from %layout
          (let ((b (%picture-block path note width font)))
            (setf (blk-id b) (operandi-gui:msg-id msg) (blk-kind b) :file (blk-text b) note)
            b)))))
  (let* ((kind (operandi-gui:msg-kind msg))
         (raw (operandi-gui:msg-text msg))
         (text (if (eq kind :file) (%file-caption raw) raw))
         (think (eq kind :think))
         (size (if think +think-size+ +size+))
         (lh (if think +think-lh+ +lh+))
         (inner (if think
                    (- width (* 2 +margin+))
                    (- (floor (* width 82) 100) (* 2 +pad-x+))))
         (rows (loop for line in (%plain text)
                     nconc (%wrap (%drawable line font) (max 40 inner) size font)))
         ;; the working notes, folded: their first lines and how many more, a tap away
         (rows (if (and think (> (length rows) (1+ +think-rows+))
                        (not (gethash (operandi-gui:msg-id msg) *expanded*)))
                   (append (subseq rows 0 +think-rows+)
                           (list (format nil "... ~d more line~:p -- tap to show" (- (length rows) +think-rows+) (- (length rows) +think-rows+))))
                   rows))
         (tw (or (loop for r in rows maximize (glass:text-width r :size size :font font)) 0)))
    (multiple-value-bind (side ink bg)
        (case kind
          (:you (values :right +you-ink+ +you-bg+))
          (:think (values :none +think-ink+ nil))
          (:err (values :left +err-ink+ +err-bg+))
          (t (values :left +bot-ink+ +bot-bg+)))
      (make-blk :side side :lines rows :size size :lh lh :ink ink :bg bg
                :id (operandi-gui:msg-id msg) :kind kind :text text
                :w (if think tw (+ tw (* 2 +pad-x+)))
                :h (if think (* lh (length rows)) (+ (* lh (length rows)) (* 2 +pad-y+)))))))

(defun %picture-block (path note width font)
  "A picture's bubble: its thumbnail (a placeholder while it is made), the NOTE under it."
  (let* ((th (%thumb path))
         (pic (and (not (keywordp th)) th))
         (pw (if pic (glass:fb-width pic) 200)) (ph (if pic (glass:fb-height pic) 150))
         (inner (min (- (floor (* width 82) 100) (* 2 +pad-x+)) +pic-max-w+))
         (rows (append (cond ((eq th :pending) (list "Picture..."))
                             ((eq th :failed) (list (format nil "A picture (~a) that did not decode."
                                                            (file-namestring path)))))
                       (and note (loop for line in (%plain note)
                                       nconc (%wrap (%drawable line font) (max 40 inner) +size+ font)))))
         (tw (max pw (or (loop for r in rows maximize (glass:text-width r :size +size+ :font font)) 0))))
    (make-blk :side :left :lines rows :size +size+ :lh +lh+ :ink +bot-ink+ :bg +bot-bg+
              :pic pic :path path
              :w (+ (min tw inner (max pw inner)) (* 2 +pad-x+))
              :h (+ (if pic (+ ph (if rows 6 0)) 0) (* +lh+ (length rows)) (* 2 +pad-y+)))))

(defun %rrect (fb x y w h r color)
  "A filled rectangle with its corners rounded to R."
  (let ((r (min r (floor w 2) (floor h 2))))
    (glass:fb-rect fb x (+ y r) w (- h (* 2 r)) color)
    (dotimes (i r)
      ;; row I from the top (and from the bottom): inset by where the corner circle is
      (let* ((dy (- r i 0.5))
             (in (round (- r (sqrt (max 0 (- (* r r) (* dy dy))))))))
        (glass:fb-rect fb (+ x in) (+ y i) (- w (* 2 in)) 1 color)
        (glass:fb-rect fb (+ x in) (+ y h -1 (- i)) (- w (* 2 in)) 1 color)))))

;;; ------------------------------------------------------------------ the window

(defparameter +settings-w+ 76 "The bar's Settings button: its width, at the bar's right.")
(defparameter +chats-w+ 56 "The bar's Chats button, left of Settings.")
(defparameter +tint+ #x6ea8fe)

(defun %draw-bar (fb)
  (let* ((w (glass:fb-width fb)) (h (glass:fb-height fb)) (y (- h +bar-h+)))
    (glass:fb-rect fb 0 y w +bar-h+ +bar-bg+)
    (%rrect fb 8 (+ y 7) (max 0 (- w 16 +settings-w+ +chats-w+)) (- +bar-h+ 14) 17 +field-bg+)
    (glass:fb-text fb 22 (+ y 15) (if (operandi-gui:busy-p) "Steer..." "Message...")
                   :size 16 :color +hint+)
    (glass:fb-text fb (+ (- w +settings-w+ +chats-w+) 6) (+ y 16) "Chats" :size 15 :color +tint+)
    (glass:fb-text fb (+ (- w +settings-w+) 2) (+ y 16) "Settings" :size 15 :color +tint+)))

(defun %hold-menu (b)
  "A press held on a message: Copy its text, or Share a picture (Save Image is in the share sheet)."
  (let* ((path (blk-path b))
         (text (blk-text b))
         (a (kiln-native:app-alert nil (if path
                                          (file-namestring path)
                                          (let ((s (substitute #\Space #\Newline (or text ""))))
                                            (if (> (length s) 90) (concatenate 'string (subseq s 0 90) "...") s)))
                                   :sheet t))
         (actions '()))
    (when (and (stringp text) (plusp (length text)))
      (kiln-native:app-alert-action a "Copy")
      (push (lambda () (kiln-native:app-copy text)) actions))
    (when path
      (kiln-native:app-alert-action a "Share...")
      (push (lambda () (kiln-native:app-share-file path)) actions))
    (setf actions (nreverse actions))
    (kiln-native:app-alert-action a "Cancel" :style :cancel)
    (kiln-native:on-event :alert a (lambda (v h) (declare (ignore h))
                                     (let ((fn (and (consp v) (nth (first v) actions))))
                                       (when fn (funcall fn)))))
    (when actions (kiln-native:app-alert-show a))))

(defun operandi-app (fb)
  "The desk window: the conversation above, the message bar below."
  (ensure-started)
  (let ((font (glass:default-font))
        (cache (make-hash-table))       ; msg-id -> (text width epoch . blk)
        (epoch -1)                      ; *THUMB-EPOCH* when last drawn
        (hits '())                      ; ((y0 y1 blk) ...) what is on screen, for a tap or a hold
        (seen nil)                      ; what was drawn last: ((id . text) ...)
        (busy nil)
        (scroll 0)                      ; pixels up from the bottom: 0 follows what arrives
        (content-h 0)
        (dirty t)
        (press nil)                     ; (y0 scroll0 moved on-bar) while a finger is down
        (last-w 0) (last-h 0))
    (labels ((view-h () (max 1 (- (glass:fb-height fb) +bar-h+)))
             (block-of (m w)
               (let ((hit (gethash (operandi-gui:msg-id m) cache)))
                 (if (and hit (eq (first hit) (operandi-gui:msg-text m)) (= (second hit) w)
                          (= (third hit) *thumb-epoch*))
                     (cdddr hit)
                     (let ((b (%layout m w font)))
                       (setf (gethash (operandi-gui:msg-id m) cache)
                             (list* (operandi-gui:msg-text m) w *thumb-epoch* b))
                       b))))
             (clamp ()
               (setf scroll (max 0 (min scroll (max 0 (- content-h (view-h)))))))
             (redraw (msgs)
               (let* ((w (glass:fb-width fb)) (vh (view-h))
                      (blocks (mapcar (lambda (m) (block-of m w)) msgs)))
                 (setf content-h (+ +gap+ (loop for b in blocks sum (+ (blk-h b) +gap+))))
                 (clamp)
                 (setf hits '())
                 (glass:with-fb-locked (fb)
                   (glass:fb-rect fb 0 0 w vh +bg+)
                   ;; the top of the content, in window y: a short chat starts at the top, a long one
                   ;; ends at the bottom less however far it is scrolled back
                   (let ((y (if (< content-h vh) +gap+ (+ (- vh content-h) scroll +gap+))))
                     (dolist (b blocks)
                       (let ((h (blk-h b)))
                         (when (and (< y vh) (> (+ y h) 0))
                           (let* ((bx (case (blk-side b)
                                        (:right (- w +margin+ (blk-w b)))
                                        (t +margin+)))
                                  (tx (if (blk-bg b) (+ bx +pad-x+) bx))
                                  (ty (if (blk-bg b) (+ y +pad-y+) y)))
                             (when (blk-bg b) (%rrect fb bx y (blk-w b) h +radius+ (blk-bg b)))
                             (push (list (max 0 y) (min vh (+ y h)) b) hits)
                             (when (blk-pic b)
                               (glass:with-fb-clip (fb 0 0 w vh)
                                 (glass:fb-blit-clipped fb (blk-pic b) tx ty))
                               (incf ty (+ (glass:fb-height (blk-pic b)) 6)))
                             (dolist (line (blk-lines b))
                               (when (and (< ty vh) (> (+ ty (blk-lh b)) 0))
                                 (glass:fb-text fb tx (+ ty (floor (- (blk-lh b) (* 1.2 (blk-size b))) 2))
                                                line :size (blk-size b) :color (blk-ink b) :font font))
                               (incf ty (blk-lh b)))))
                         (incf y (+ h +gap+)))))
                   ;; the bar last: a glyph that ran over the view's bottom is painted out
                   (%draw-bar fb)))))
      (values
       ;; keys: none -- the alert takes the text
       (lambda (down ks) (declare (ignore down ks)) nil)
       (lambda (mask x y)
         (let ((on-bar (>= y (view-h))))
           (cond ((logtest mask 1)
                  (if (null press)
                      (setf press (list y scroll nil on-bar (get-internal-real-time)))
                      (destructuring-bind (y0 s0 moved bar t0) press
                        (declare (ignore bar t0))
                        (when (or moved (> (abs (- y y0)) +tap-slop+))
                          (setf (third press) t
                                scroll (+ s0 (- y y0)))
                          (clamp)
                          (setf dirty t)))))
                 (press
                  (destructuring-bind (y0 s0 moved bar t0) press
                    (declare (ignore y0 s0))
                    (setf press nil)
                    (cond ((and bar on-bar (not moved))
                           (cond ((>= x (- (glass:fb-width fb) 8 +settings-w+)) (open-settings))
                                 ((>= x (- (glass:fb-width fb) 8 +settings-w+ +chats-w+)) (open-chats))
                                 (t (compose))))
                          ((and (not bar) (not moved))
                           (let* ((hit (find-if (lambda (h) (<= (first h) y (second h))) hits))
                                  (b (and hit (third hit)))
                                  (held (>= (- (get-internal-real-time) t0)
                                            (* 0.45 internal-time-units-per-second))))
                             (when b
                               (cond (held (%hold-menu b))
                                     ;; a tap on a picture: the whole of it, in Quick Look
                                     ((blk-path b) (kiln-native:app-preview (blk-path b)))
                                     ;; on the working notes: open them, or fold them again
                                     ((eq (blk-kind b) :think)
                                      (let ((id (blk-id b)))
                                        (if (gethash id *expanded*) (remhash id *expanded*) (setf (gethash id *expanded*) t)))
                                      (incf *thumb-epoch*))))))))))
           t))
       (lambda ()
         (let* ((msgs (operandi-gui:rows))
                (now (mapcar (lambda (m) (cons (operandi-gui:msg-id m) (operandi-gui:msg-text m))) msgs))
                (b (operandi-gui:busy-p)))
           (unless (and (= last-w (glass:fb-width fb)) (= last-h (glass:fb-height fb)))
             (setf last-w (glass:fb-width fb) last-h (glass:fb-height fb) dirty t))
           (unless (and (equal now seen) (eq b busy) (= epoch *thumb-epoch*))
             (setf seen now busy b epoch *thumb-epoch* dirty t))
           (when dirty
             (setf dirty nil)
             (redraw msgs)
             t)))
       nil nil
       ;; not a text window: the desk must not raise the keyboard for it (the alert has its own)
       nil))))

;;; ================================================================== INFERENCE SETTINGS
;;;
;;; Where the agent's model runs, asked the first time the window opens and changed later with the bar's
;;; Settings (or /settings).  Two native steps, because a native form's fields are fixed when it is made:
;;;
;;;   1. an action sheet -- which backend: OpenRouter, PPQ, Routstr, Tinfoil, Maple, or a local server;
;;;   2. that backend's form -- its key, its model, its node or address, Confidential and the --trust names
;;;      that apply to it, reasoning effort -- whose Connect hands them to OPERANDI-GUI:CONFIGURE!.
;;;
;;; CONFIGURE! checks the choice before keeping it (a key; --confidential on a sealed path whose operators
;;; TRUST names) and refuses with a reason, which comes back as an alert offering the form again.  A choice
;;; it keeps is then PINGED (OPERANDI-GUI:PREFLIGHT!, on the worker, so the window keeps drawing): one token
;;; asked of the model, and its answer, or the provider's reason, said in the chat.
;;;
;;; THE KEYS ARE IN THE KEYCHAIN, one per backend ("operandi.ppq" ...), through natrium's gossamer:
;;; KEYCHAIN-KEEP hands it over from fixed memory, KEYCHAIN-RECALL reads it back into fixed memory.  A key
;;; is never written to Documents or shown again: the form's key field comes up empty, and left empty it
;;; means "the one I gave you".  The rest -- backend, model, address, the switches -- is not secret and is
;;; kept in Documents/operandi/settings.json.

(defparameter +backends+
  ;; (key title default-model blurb)
  '((:openrouter "OpenRouter" "z-ai/glm-5.3-flash"
     "OpenRouter routes to hundreds of hosted models.  Requests are not sealed: the provider sees them.")
    (:ppq "PPQ" "private/gpt-oss-120b"
     "PPQ: pay-per-query hosted models.  With Confidential on, a private/* model is sealed to an AMD-attested Tinfoil enclave, verified on this phone.")
    (:routstr "Routstr" "gemini-3-6-flash"
     "Routstr: independent nodes paid with Cashu eCash or an sk- key.  A tinfoil-* model is sealed to an attested Tinfoil enclave and the node relays it blind.")
    (:tinfoil "Tinfoil" "gpt-oss-120b"
     "Tinfoil's own enclave: every request is sealed to an AMD SEV-SNP enclave verified on this phone.  There is no plaintext mode.")
    (:maple "Maple" "gpt-oss-120b"
     "Maple's enclave on AWS Nitro: every request is encrypted to a key in an attestation verified on this phone.")
    (:local "Local server" ""
     "An OpenAI-compatible server you run: illatio, llama.cpp.  Nothing leaves your network."))
  "The backends the panel offers, in the sheet's order.")

(defparameter +trust-for+
  '((:ppq "tinfoil-untrusted") (:routstr "tinfoil-untrusted") (:tinfoil "tinfoil-untrusted")
    (:maple "maple-routing" "maple-inference" "tinfoil-untrusted" "continuum"))
  "The --trust names that can matter on each backend: what Confidential's check may ask for.")

(defparameter +efforts+ '(nil :off :low :medium :high))
(defparameter +effort-names+ '("Default" "Off" "Low" "Medium" "High"))

(defun %backend (key) (assoc key +backends+))
(defun %settings-path () (concatenate 'string *root* "settings.json"))

;;; ---- the keychain

(defun %key-name (backend) (format nil "operandi.~(~a~)" backend))

(defun keep-key (backend key)
  "Hand KEY to the keychain for BACKEND.  T if it took it."
  (and (gossamer:keychain-available-p)
       (let ((octets (map '(vector (unsigned-byte 8)) (lambda (c) (logand #xff (char-code c))) key)))
         (prog1 (gossamer:keychain-keep (%key-name backend) octets)
           (fill octets 0)))))

(defun recall-key (backend)
  "The key kept for BACKEND, as a string, or NIL."
  (when (gossamer:keychain-available-p)
    (let ((b (ignore-errors (gossamer:keychain-recall (%key-name backend)))))
      (when b
        (prog1 (map 'string #'code-char b)
          (ignore-errors (gossamer:free b)))))))

;;; ---- settings.json

(defun load-settings ()
  "The saved settings as a plist (:backend :model :url :confidential :trust :effort), or NIL."
  (let ((h (ignore-errors
            (with-open-file (in (%settings-path) :if-does-not-exist nil)
              (and in (json-simple:parse (let ((s (make-string (file-length in))))
                                           (subseq s 0 (read-sequence s in)))))))))
    (when (hash-table-p h)
      (let ((b (find (gethash "backend" h) +backends+ :key (lambda (e) (string-downcase (symbol-name (first e))))
                                                       :test #'equal)))
        (when b
          (list :backend (first b)
                :model (gethash "model" h)
                :url (gethash "url" h)
                :confidential (eq (gethash "confidential" h) t)
                :trust (coerce (or (gethash "trust" h) #()) 'list)
                :effort (let ((e (gethash "effort" h)))
                          (and (stringp e) (find e (rest +efforts+) :key #'symbol-name :test #'string-equal)))))))))

(defun save-settings (plist)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "backend" h) (string-downcase (symbol-name (getf plist :backend)))
          (gethash "model" h) (or (getf plist :model) "")
          (gethash "url" h) (or (getf plist :url) "")
          (gethash "confidential" h) (and (getf plist :confidential) t)
          (gethash "trust" h) (coerce (getf plist :trust) 'vector)
          (gethash "effort" h) (if (getf plist :effort) (string-downcase (symbol-name (getf plist :effort))) ""))
    (handler-case
        (progn
          (ensure-directories-exist (%settings-path))
          (with-open-file (out (%settings-path) :direction :output :if-exists :supersede)
            (write-string (json-simple:stringify h) out))
          (format t "~&operandi: settings saved (~a)~%" (%settings-path))
          t)
      (error (e) (format t "~&operandi: settings NOT saved to ~a: ~a~%" (%settings-path) e) nil))))

;;; ---- applying them

(defun %note (text)
  "A line from the settings in the chat, where the agent's own notes go."
  (funcall (find-symbol "ADD-MSG" "OPERANDI-GUI") "operandi" text :bot))

(defvar *last-applied* nil "The settings plist last applied, for the preflight's answer.")

(defun apply-settings (plist &key quiet)
  "Give PLIST to CONFIGURE! with the backend's key from the keychain.  On success the settings are saved
   and the model pinged; on refusal (values NIL reason).  QUIET: at startup, say nothing unless it fails."
  (let ((backend (getf plist :backend)))
    (multiple-value-bind (ok what)
        (operandi-gui:configure! :backend backend
                                 :token (and (not (eq backend :local)) (or (getf plist :typed-key) (recall-key backend)))
                                 :model (getf plist :model) :url (getf plist :url)
                                 :confidential (getf plist :confidential) :trust (getf plist :trust)
                                 :effort (getf plist :effort))
      (%sync-images)
      (cond (ok (save-settings plist)
                (setf *last-applied* (list* :quiet quiet :label what plist))
                (unless quiet (%note (format nil "Checking ~a..." what)))
                (operandi-gui:preflight!)
                (values t what))
            (t (when quiet (%note (format nil "The saved settings did not apply: ~a  Tap Settings to fix them." what)))
               (values nil what))))))

(defun %preflight-answer (ok reason)
  "The worker's ping came back."
  (let ((label (getf *last-applied* :label)) (quiet (getf *last-applied* :quiet)))
    (cond ((and ok quiet))
          (ok (%note (format nil "Connected: ~a." label)))
          (t
           (let ((a (kiln-native:app-alert "The model did not answer"
                                           (format nil "~a~%~%~a" label (or reason "no reason given")))))
             (kiln-native:app-alert-action a "Keep" :style :cancel)        ; 0
             (kiln-native:app-alert-action a "Edit Settings")              ; 1
             (kiln-native:on-event :alert a (lambda (v h) (declare (ignore h))
                                              (when (and (consp v) (eql (first v) 1))
                                                (backend-form (getf *last-applied* :backend) *last-applied*))))
             (kiln-native:app-alert-show a))))))

;;; ---- the panel

(defun %phone-words (why)
  "operandi's reasons name its command-line flags; here they are the form's switches."
  (let ((s why))
    (flet ((swap (from to)
             (loop for p = (search from s) while p
                   do (setf s (concatenate 'string (subseq s 0 p) to (subseq s (+ p (length from))))))))
      (swap "which --trust does not name; declare --trust=" "which you have not trusted. Turn on Trust ")
      (swap "--trust does not name" "you have not trusted")
      (swap "Declare --trust=" "Turn on Trust ")
      (swap "--confidential" "Confidential")
      (swap "--trust" "Trust")
      (swap "--maple ADDR" "a Maple proxy")
      (swap "--maple MODEL" "Maple's enclave"))
    s))

(defun open-settings (&key first-run)
  "Step 1: which backend."
  (let ((a (kiln-native:app-alert (if first-run "Welcome to operandi" "Inference")
                                  (if first-run
                                      "Choose where operandi's model runs.  You can change it later with Settings."
                                      "Where should operandi's model run?")
                                  :sheet t)))
    (dolist (b +backends+) (kiln-native:app-alert-action a (second b)))
    (kiln-native:app-alert-action a "Cancel" :style :cancel)
    (kiln-native:on-event :alert a
                          (lambda (v h) (declare (ignore h))
                            (let ((i (and (consp v) (first v))))
                              (when (and (integerp i) (< -1 i (length +backends+)))
                                (backend-form (first (nth i +backends+)))))))
    (kiln-native:app-alert-show a)))

(defun backend-form (backend &optional prior)
  "Step 2: BACKEND's settings, filled from PRIOR (a settings plist: what was typed before a refusal), else
   the saved ones when they are this backend's, else its defaults."
  (destructuring-bind (key title default-model blurb) (%backend backend)
    (let* ((saved (or prior (let ((s (load-settings))) (and (eq (getf s :backend) key) s))))
           (local (eq key :local))
           (has-key (and (not local) (recall-key key)))
           (trust-names (rest (assoc key +trust-for+)))
           (form (kiln-native:app-form title :save "Connect"))
           (fields '()))
      (flet ((field (name &rest args)
               (push (cons name (apply #'kiln-native:app-form-field form args)) fields)))
        (unless local
          (field :key "API key" "" :kind :secure
                                   :placeholder (if has-key "Saved in Keychain" "Required")))
        (when local
          (field :url "Server" (or (getf saved :url)
                                   (let ((u (uiop:getenv "OPERANDI_CHAT_URL")))
                                     (and u (plusp (length u)) (operandi-gui:url-label u)))
                                   "127.0.0.1:9931")
                 :kind :url :placeholder "host:port"))
        (when (eq key :routstr)
          (field :url "Node" (or (getf saved :url) "") :kind :url :placeholder "the default node"))
        (field :model "Model" (or (getf saved :model) default-model) :kind :text
                              :placeholder (if local "what the server serves" default-model))
        (when (member key '(:ppq :routstr :tinfoil :maple))
          (field :confidential "Confidential" (if (getf saved :confidential) "1" "0") :kind :switch))
        (dolist (n trust-names)
          (field (cons :trust n) (format nil "Trust ~a" n)
                 (if (member n (getf saved :trust) :test #'string=) "1" "0") :kind :switch))
        (field :effort "Reasoning" (princ-to-string (or (position (getf saved :effort) +efforts+) 0))
               :kind :choice :choices +effort-names+))
      (setf fields (nreverse fields))
      (when (eq key :ppq) (ppq-buttons form))
      (kiln-native:app-form-note
       form (format nil "~a~@[~%~%~a~]~:[~;~%~%Your key is kept in this phone's Keychain; leave the field empty to keep the saved one.~]"
                    blurb
                    (and trust-names
                         "Confidential refuses any request that is not sealed to an enclave verified here.  Trusting a party is a statement, not a check: name only operators you accept seeing your prompts.")
                    (not local)))
      (kiln-native:on-event
       :form form
       (lambda (v h) (declare (ignore h))
         (when (and (consp v) (eql (first v) 1))
           (let ((vals (rest v)) (plist (list :backend key)) (trust '()))
             (flet ((val (name) (let ((f (cdr (assoc name fields :test #'equal)))) (and f (nth f vals)))))
               (let ((typed (val :key)))
                 (when (and typed (plusp (length (string-trim " " typed))))
                   (unless (keep-key key (string-trim " " typed))
                     (%note "The keychain did not take the key; it is used for now but will not be remembered."))
                   ;; used even if the keychain refused it: CONFIGURE! gets it through RECALL-KEY, so fall back
                   (unless (recall-key key) (setf (getf plist :typed-key) (string-trim " " typed)))))
               (setf (getf plist :model) (string-trim " " (or (val :model) ""))
                     (getf plist :url) (string-trim " " (or (val :url) ""))
                     (getf plist :confidential) (equal (val :confidential) "1")
                     (getf plist :effort) (nth (or (ignore-errors (parse-integer (val :effort))) 0) +efforts+))
               (dolist (n trust-names)
                 (when (equal (val (cons :trust n)) "1") (push n trust)))
               (setf (getf plist :trust) (nreverse trust)))
             (multiple-value-bind (ok why) (apply-settings plist)
               (unless ok
                 (let ((a (kiln-native:app-alert "Those settings did not apply" (%phone-words why))))
                   (kiln-native:app-alert-action a "Cancel" :style :cancel)
                   (kiln-native:app-alert-action a "Edit")
                   (kiln-native:on-event :alert a (lambda (v h) (declare (ignore h))
                                                    (when (and (consp v) (eql (first v) 1))
                                                      (backend-form key plist))))
                   (kiln-native:app-alert-show a))))))))
      (kiln-native:app-form-show form))))


;;; ================================================================== BACKGROUND WORK
;;;
;;; A network call made here would stop the desk until it answered.  BACKGROUND runs FN on a thread of its
;;; own and THEN, here, with what it returned: the thread shares nothing with this one (modus threads share
;;; no state), so its answer comes back as a message -- operandi-gui's mailbox -- read each frame.  AFTER is
;;; a timer on the same frame hook.

(defvar *jobs* '() "(mailbox . then) for each BACKGROUND call not yet answered.")
(defvar *timers* '() "(internal-real-time . fn) to run once that time has come.")

(defun %pump-jobs ()
  (when *jobs*
    (dolist (j (copy-list *jobs*))
      (multiple-value-bind (v ok) (operandi-gui::mb-try (car j))
        (when ok
          (setf *jobs* (remove j *jobs*))
          (funcall (cdr j) v)))))
  (when *timers*
    (let ((now (get-internal-real-time)))
      (dolist (tm (copy-list *timers*))
        (when (>= now (car tm))
          (setf *timers* (remove tm *timers*))
          (funcall (cdr tm)))))))

(defun %ensure-pump ()
  (pushnew '%pump-jobs (symbol-value (find-symbol "*KILN-FRAME-HOOKS*" "CL-USER"))))

(defun background (fn then)
  "Run FN on a thread; THEN gets (:OK . values) or (:ERROR message), back here."
  (%ensure-pump)
  (let ((box (operandi-gui::make-mailbox "kiln-operandi job")))
    (push (cons box then) *jobs*)
    (bt:make-thread (lambda ()
                      (operandi-gui::mb-send box (handler-case (cons :ok (multiple-value-list (funcall fn)))
                                                   (error (e) (list :error (princ-to-string e))))))
                    :name "kiln-operandi job")
    t))

(defun after (seconds fn)
  (%ensure-pump)
  (push (cons (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))) fn) *timers*))

(defun %alert (title message &rest buttons)
  "An alert with BUTTONS ((title . fn-or-nil) ...), the first the Cancel one."
  (let ((a (kiln-native:app-alert title message)))
    (loop for (b . nil) in buttons for i from 0
          do (kiln-native:app-alert-action a b :style (if (zerop i) :cancel :default)))
    (kiln-native:on-event :alert a (lambda (v h) (declare (ignore h))
                                     (let ((fn (and (consp v) (cdr (nth (first v) buttons)))))
                                       (when fn (funcall fn)))))
    (kiln-native:app-alert-show a)))

;;; ================================================================== PPQ: AN ACCOUNT, AND CREDIT FOR IT
;;;
;;; PPQ needs no sign-up: POST /accounts/create answers an API key and a credit id, and the account is paid
;;; for with Lightning.  Both go to the keychain ("operandi.ppq", the key CONFIGURE! is given, and
;;; "operandi.ppq-credit", which the balance names the account by).  Top Up asks for an amount, shows PPQ's
;;; invoice natively -- its QR code, Copy, Open in Wallet -- and checks it every few seconds until it is paid,
;;; expires, or is closed.

(defparameter +topups+ '(1000 5000 20000 100000) "Sats the Top Up sheet offers.")
(defparameter +paid-words+ '("paid" "completed" "complete" "settled" "success" "succeeded"))

(defun %sats (n) (format nil "~:d sats" n))

(defun ppq-buttons (form)
  (kiln-native:on-event :tap (kiln-native:app-form-button form "Create PPQ Account")
                        (lambda (v h) (declare (ignore v h)) (ppq-create)))
  (kiln-native:on-event :tap (kiln-native:app-form-button form "Top Up with Lightning")
                        (lambda (v h) (declare (ignore v h)) (ppq-topup)))
  (kiln-native:on-event :tap (kiln-native:app-form-button form "Balance")
                        (lambda (v h) (declare (ignore v h)) (ppq-show-balance))))

(defun ppq-create (&key force)
  (if (and (not force) (recall-key :ppq))
      (%alert "Replace the saved PPQ key?"
              "A new account starts empty.  Any credit on the current one stays there, and this phone will no longer have its key."
              (cons "Cancel" nil)
              (cons "Create New Account" (lambda () (ppq-create :force t))))
      (background (lambda () (operandi.payments:ppq-new-account))
                  (lambda (r)
                    (if (eq (first r) :error)
                        (%alert "PPQ did not create an account" (second r) (cons "OK" nil))
                        (destructuring-bind (key cid) (rest r)
                          (let ((kept (and (keep-key :ppq key)
                                           (or (null cid) (keep-key :ppq-credit cid)))))
                            (%sync-images)
                            (%alert "PPQ account created"
                                    (if kept
                                        "Its key is in this phone's Keychain.  It has no credit yet: top it up with Lightning, then Connect."
                                        "The keychain did not take its key, so it will not be remembered.")
                                    (cons "Later" nil)
                                    (cons "Top Up" #'ppq-topup)))))))))

(defun ppq-topup ()
  (let ((key (recall-key :ppq)))
    (if (null key)
        (%alert "No PPQ account yet"
                "Create one here, or enter your key and Connect; then top it up."
                (cons "OK" nil))
        (let ((a (kiln-native:app-alert "Top up PPQ" "Pay with any Lightning wallet." :sheet t)))
          (dolist (n +topups+) (kiln-native:app-alert-action a (%sats n)))
          (kiln-native:app-alert-action a "Cancel" :style :cancel)
          (kiln-native:on-event :alert a
                                (lambda (v h) (declare (ignore h))
                                  (let ((n (and (consp v) (nth (first v) +topups+))))
                                    (when n (ppq-invoice n key)))))
          (kiln-native:app-alert-show a)))))

(defun ppq-invoice (sats key)
  (background (lambda () (operandi.payments:create-ppq-topup sats :token key))
              (lambda (r)
                (if (eq (first r) :error)
                    (%alert "PPQ did not make an invoice" (second r) (cons "OK" nil))
                    (let* ((inv (second r))
                           (h (kiln-native:app-invoice "Top Up PPQ" (%sats sats) (getf inv :bolt11)))
                           (open t))
                      (kiln-native:on-event :invoice h (lambda (v hh) (declare (ignore v hh)) (setf open nil)))
                      (labels ((check ()
                                 (when open
                                   (background
                                    (lambda () (getf (operandi.payments:ppq-topup-status (getf inv :invoice-id) :token key)
                                                     :status))
                                    (lambda (r)
                                      (let ((st (and (eq (first r) :ok) (second r))))
                                        (cond ((not open))
                                              ((and (stringp st) (member st +paid-words+ :test #'string-equal))
                                               (kiln-native:app-invoice-status h "Paid -- the credit is on your account." :paid t)
                                               (%note (format nil "PPQ: ~a added." (%sats sats)))
                                               (ppq-balance-then (lambda (text)
                                                                   (when (and open text)
                                                                     (kiln-native:app-invoice-status
                                                                      h (format nil "Paid.  Balance: ~a" text) :paid t)))))
                                              ((and (stringp st) (member st '("expired" "failed" "cancelled" "canceled")
                                                                         :test #'string-equal))
                                               (kiln-native:app-invoice-status h (format nil "The invoice ~(~a~).  Close it and top up again." st)))
                                              (t (after 4 #'check)))))))))
                        (after 4 #'check)))))))

(defun %balance-text (h)
  "PPQ's balance answer as a line: the numbers in it, by name."
  (if (and (hash-table-p h) (realp (gethash "balance" h)))
      (format nil "~,2f" (gethash "balance" h))
  (if (hash-table-p h)
      (let ((parts '()))
        (maphash (lambda (k v)
                   (when (realp v)
                     (push (if (integerp v)
                               (format nil "~a ~d" (substitute #\Space #\_ k) v)
                               (format nil "~a ~,2f" (substitute #\Space #\_ k) v))
                           parts)))
                 h)
        (if parts (format nil "~{~a~^, ~}" (sort parts #'string<)) "no balance in PPQ's answer"))
      "no answer")))

(defun ppq-balance-then (fn)
  (let ((cid (let ((b (ignore-errors (gossamer:keychain-recall (%key-name :ppq-credit)))))
               (when b (prog1 (map 'string #'code-char b) (ignore-errors (gossamer:free b)))))))
    (if (null cid)
        (funcall fn nil)
        (background (lambda () (operandi.payments:ppq-balance :credit-id cid))
                    (lambda (r) (funcall fn (and (eq (first r) :ok) (%balance-text (second r)))))))))

(defun ppq-show-balance ()
  (if (null (recall-key :ppq))
      (%alert "No PPQ account yet" "Create one here, or enter your key and Connect." (cons "OK" nil))
      (ppq-balance-then (lambda (text)
                          (%alert "PPQ balance"
                                  (or text "This phone has no credit id for the account (it was entered as a key, not created here).")
                                  (cons "OK" nil))))))
