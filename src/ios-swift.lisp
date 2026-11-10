;;;; ios-swift.lisp -- Lisp calls Swift.  A Swift function is named in host/ios/modus-swift.m's table
;;;; (by the Swift side, in host/ios/swift/), and SWIFT-CALL calls it by that name with words: integers, and the
;;;; addresses of byte arrays that hold UTF-8 text or an encoded term.  The native components are made in Swift
;;;; and driven from here.  The data Lisp sends a list is a TERM (net/term.lisp, the wire format), and the events
;;;; Swift sends back are terms too, decoded here by the same decoder as the actor messages.  SWIFT-PUMP runs
;;;; each frame (the app's frame hooks).
;;;;
;;;;   (ui-label "text" x y w h)              (ui-button "title" x y w h)
;;;;   (ui-switch on x y)                     (ui-slider value x y w h)
;;;;   (ui-text-field "placeholder" x y w h)  (ui-panel x y w h rgb)   ; w 0: the whole screen
;;;;   (ui-stack vertical-p spacing x y w h)  (ui-scroll x y w h)  (ui-image "star.fill" x y w h)
;;;;   (ui-add parent child)                  (ui-list x y w h)  (ui-list-set-data h rows)
;;;;   (ui-set-text h "text")  (ui-set-value h v)  (ui-text-value h)  (ui-set-frame h x y w h)
;;;;   (ui-remove h)  (ui-tap h)  (ui-resign)
;;;;   (on-event kind h (lambda (value h) ...))   ; kind :tap :switch :slider :text :select

(defpackage #:kiln-swift
  (:use #:cl)
  (:export #:swift-call #:ui-label #:ui-button #:ui-switch #:ui-slider #:ui-text-field #:ui-panel
           #:ui-stack #:ui-scroll #:ui-image #:ui-add #:ui-list #:ui-list-set-data
           #:ui-set-text #:ui-set-value #:ui-text-value #:ui-set-frame #:ui-remove #:ui-tap #:ui-resign
           #:on-event #:on-tap #:swift-pump))

(in-package #:kiln-swift)

(defparameter *frame-bytes* 256)
(defparameter *name-at* 128)

(defun %addr (array)
  (+ (modus-int::%gc-word-of array (modus-int::%conv-addr #x100050A0)) 7))

(defun %octets (text)
  (coerce (seal:utf8-encode text) '(simple-array (unsigned-byte 8) (*))))

(defun %term-octets (term)
  "TERM encoded as the wire bytes, in an array of exactly their size."
  (let* ((n (cl-user::term-size term))
         (b (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (cl-user::term-encode term (%addr b))
    b))

(defun %put-word (f off w)
  (dotimes (i 8) (setf (aref f (+ off i)) (ldb (byte 8 (* 8 i)) w))))

(defun %get-word (f off)
  (let ((w 0))
    (dotimes (i 8 w)
      (setf w (+ w (ash (aref f (+ off i)) (* 8 i)))))))

(defun swift-call (name &rest args)
  "Call the Swift function NAME with ARGS (at most six words); the word it returns."
  (when (> (length args) 6) (error "swift-call: at most six arguments, not ~D" (length args)))
  (let ((f (make-array *frame-bytes* :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for c across name for i from *name-at*
          do (setf (aref f i) (logand (char-code c) #xff)))
    (%put-word f 0 (length args))
    (loop for a in args for i from 0 do (%put-word f (+ 8 (* 8 i)) (or a 0)))
    (let ((rc (cl-user::%kiln-sys 1060 (%addr f) 0 0)))
      (when (< rc 0) (error "swift-call: no Swift function named ~A" name))
      (%get-word f 64))))

;;; The components.  A handle is the Swift side's name for a view.

(defun ui-label (text x y w h)
  (let ((b (%octets text)))
    (swift-call "ui_label_new" (%addr b) (length b) x y w h)))

(defun ui-button (title x y w h)
  (let ((b (%octets title)))
    (swift-call "ui_button_new" (%addr b) (length b) x y w h)))

(defun ui-switch (on x y)
  (swift-call "ui_switch_new" (if on 1 0) x y))

(defun ui-slider (value x y w h)
  (swift-call "ui_slider_new" value x y w h))

(defun ui-text-field (placeholder x y w h)
  (let ((b (%octets placeholder)))
    (swift-call "ui_text_field_new" (%addr b) (length b) x y w h)))

(defun ui-panel (x y w h rgb)
  (swift-call "ui_panel_new" x y w h rgb))

(defun ui-stack (vertical-p spacing x y w h)
  "A stack: its children laid out in a column (VERTICAL-P) or a row, SPACING points apart."
  (swift-call "ui_stack_new" (if vertical-p 0 1) spacing x y w h))

(defun ui-scroll (x y w h)
  "A scroll view: the children added to it stack vertically and scroll."
  (swift-call "ui_scroll_new" x y w h))

(defun ui-image (name x y w h)
  "An SF Symbol by NAME (\"star.fill\"), in white."
  (let ((b (%octets name)))
    (swift-call "ui_image_new" (%addr b) (length b) x y w h)))

(defun ui-add (parent child)
  "Put CHILD into PARENT: a stack's arranged children, a scroll view's content, or a view's subviews."
  (swift-call "ui_add" parent child))

(defun ui-list (x y w h)
  "A list of rows: set its rows with UI-LIST-SET-DATA.  The table makes its cells as they scroll into view; a
   tap on a row is a :select event with the row's index."
  (swift-call "ui_list_new" x y w h))

(defun ui-list-set-data (handle rows)
  "ROWS: a list of plists, each (:title \"...\" :subtitle \"...\") -- the list's rows, in order, as a term."
  (let ((b (%term-octets rows)))
    (swift-call "ui_list_set_data" handle (%addr b) (length b))))

(defun ui-set-text (handle text)
  (let ((b (%octets text)))
    (swift-call "ui_set_text" handle (%addr b) (length b))))

(defun ui-set-value (handle value)
  "A switch's state (T or NIL), or a slider's value."
  (swift-call "ui_set_value" handle (if (eq value t) 1 (if (null value) 0 value))))

(defun ui-text-value (handle)
  "The text of a text field (or label), as a string."
  (let* ((cap 1024)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (swift-call "ui_text_value" handle (%addr buf) cap)))
    (cond ((zerop n) "")
          ((> n cap)
           (let ((big (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
             (swift-call "ui_text_value" handle (%addr big) n)
             (seal:utf8-decode big :end n)))
          (t (seal:utf8-decode buf :end n)))))

(defun ui-set-frame (handle x y w h)
  (swift-call "ui_set_frame" handle x y w h))

(defun ui-remove (handle)
  (swift-call "ui_remove" handle))

(defun ui-tap (handle)
  "Tap the button HANDLE, as a touch would: its event comes back through SWIFT-PUMP."
  (swift-call "ui_send_tap" handle))

(defun ui-resign ()
  "The keyboard goes away (a text field's, or the desk's)."
  (swift-call "ui_resign"))

;;; The events.  Each is a term: (:TAP h), (:SWITCH h on), (:SLIDER h value), (:TEXT h), (:SELECT h index).

(defun %next-event ()
  "The oldest event from the native side, as its term: (kind handle [value]), or NIL when there is none."
  (let* ((cap 1024)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (swift-call "ui_events_next" (%addr buf) cap)))
    (when (> n 0)
      ;; the decoder reads from the event's bytes: point its cursor (the one it reads, the world's) at them
      (setf (cl-user::mem-ref (cl-user::decode-ptr-addr) :u64) (%addr buf))
      (cl-user::term-decode-step))))

(defvar *handlers* (make-hash-table :test 'equal))

(defun on-event (kind handle fn)
  "Run FN with (value handle) when HANDLE sends an event of KIND (:tap, :switch, :slider, :text or :select)."
  (setf (gethash (list kind handle) *handlers*) fn)
  (pushnew 'swift-pump cl-user::*kiln-frame-hooks*))

(defun on-tap (handle fn)
  (on-event :tap handle fn))

(defun swift-pump ()
  "Each frame: the native events, delivered (a few at a time, so a frame is never long)."
  (loop repeat 16
        do (let ((event (%next-event)))
             (unless event (return))
             (destructuring-bind (kind handle &optional value) event
               (let ((fn (gethash (list kind handle) *handlers*)))
                 (when fn
                   (handler-case (funcall fn value handle)
                     (serious-condition (e)
                       (format t "~&event handler ~A ~A: ~A~%" kind handle e)))))))))
