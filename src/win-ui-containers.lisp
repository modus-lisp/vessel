;;;; win-ui-containers.lisp -- the Windows host's containers, filled views, shapes, images and labels: the calls the
;;;; Mac host answers with UIKit views of those kinds, answered here with Win32.  Loads after win-host.lisp.
;;;;
;;;;   filled views   ui_panel_new  ui_blur_new  ui_shape_new  ui_drawn_image_new  ui_file_image_new
;;;;   containers     ui_stack_new  ui_scroll_new  ui_paging_new  ui_zoom_scroll_new  ui_zoom_content
;;;;   images, labels ui_image_new  ui_unavailable_new  ui_attributed_label_new  ui_dynamic_label_new
;;;;   controls       ui_gauge_new  ui_crown_new
;;;;   the screen     ui_screen_width  ui_screen_height  ui_safe_area_top
;;;;
;;;; A container is a window of the class "VesselLayout", which has no background brush: it is transparent, and
;;;; what is behind shows through.  A filled view is a window of a class per colour ("VesselFill<RRGGBB>"), whose
;;;; background brush is that colour.  Scrolling, paging, zooming and the crown are WM_VSCROLL, WM_HSCROLL and
;;;; WM_MOUSEWHEEL, taken by CT-HOOK from *MESSAGE-HOOKS*.  Every helper here is named CT-, so no other file's helper
;;;; can collide with one.  Approximations are noted at each call.

(in-package #:vessel-windows)

(defvar *ct-get-window-rect* (modus-int::w32-proc "user32.dll" "GetWindowRect"))
(defvar *ct-get-scroll-info* (modus-int::w32-proc "user32.dll" "GetScrollInfo"))
(defvar *ct-set-scroll-info* (modus-int::w32-proc "user32.dll" "SetScrollInfo"))
(defvar *ct-screen-to-client* (modus-int::w32-proc "user32.dll" "ScreenToClient"))
(defvar *ct-create-elliptic-rgn* (modus-int::w32-proc "gdi32.dll" "CreateEllipticRgn"))

(defparameter *ct-layout-style* #x56000000 "WS_CHILD|WS_VISIBLE|WS_CLIPCHILDREN|WS_CLIPSIBLINGS")
(defparameter *ct-scroll-gap* 8 "Points between a scroll view's children.")
(defparameter *ct-glyphs*
  '(("star.fill" . #xE735) ("star" . #xE734) ("heart" . #xE006) ("gear" . #xE713) ("gearshape" . #xE713)
    ("house" . #xE80F) ("house.fill" . #xE80F) ("magnifyingglass" . #xE721) ("search" . #xE721)
    ("checkmark" . #xE73E) ("xmark" . #xE711) ("plus" . #xE710) ("trash" . #xE74D) ("play.fill" . #xE768)
    ("pause.fill" . #xE769) ("envelope" . #xE715) ("camera" . #xE722) ("mic" . #xE720) ("lock" . #xE72E)
    ("info" . #xE946) ("person" . #xE77B) ("person.fill" . #xE77B))
  "SF Symbol names -> their glyphs in Segoe Fluent Icons (Segoe MDL2 Assets' code points).  Others show as text.")

;;; ---- classes and windows ---------------------------------------------------------------------------------------------

(defvar *ct-layout-class* nil)
(defun ct-layout-class ()
  "The transparent containers' class: no background brush."
  (unless *ct-layout-class*
    (register-class "VesselLayout" *arrow* 0)
    (setf *ct-layout-class* "VesselLayout"))
  *ct-layout-class*)

(defvar *ct-fill-classes* (make-hash-table))
(defun ct-fill-class (rgb)
  "The class whose background is RGB (0xRRGGBB), registered once per colour."
  (or (gethash rgb *ct-fill-classes*)
      (let ((name (format nil "VesselFill~6,'0X" rgb)))
        (register-class name *arrow* (c *create-brush* (colorref rgb)))
        (setf (gethash rgb *ct-fill-classes*) name))))

(defun ct-window (parent class text style x y w h)
  "A child window of PARENT (a native window), of CLASS, with TEXT; X Y W H in pixels; STYLE's bits, and
   WS_CHILD|WS_VISIBLE.  Its font is the window's."
  (let ((hw (u *create-window* 0 (wide1 class) (wide2 text) (logior #x50000000 style) x y w h parent 0 *hinst* 0)))
    (when (zerop hw) (error "vessel-windows: no ~A" class))
    (c *send-message* hw #x30 *font* 1)                                                   ; WM_SETFONT
    hw))

(defun ct-set-font (hw font) (c *send-message* hw #x30 font 1))

(defvar *ct-fonts* (make-hash-table :test 'equal))
(defun ct-font (points weight underline face)
  "A font of POINTS high (its character height), WEIGHT, underlined if UNDERLINE (0 or 1), named FACE."
  (let ((key (list points weight underline face)))
    (or (gethash key *ct-fonts*)
        (setf (gethash key *ct-fonts*)
              (c *create-font* (- (px points)) 0 0 0 weight 0 underline 0 1 0 0 5 0 (wide1 face))))))

;;; ---- geometry (pixels) -----------------------------------------------------------------------------------------------

(defun ct-win-rect (hw)
  "HW's rectangle on the screen: left, top, right, bottom."
  (let ((r (rect-area)))
    (c *ct-get-window-rect* hw r)
    (values (s32 (peek32 r)) (s32 (peek32 (+ r 4))) (s32 (peek32 (+ r 8))) (s32 (peek32 (+ r 12))))))

(defun ct-w (hw) (multiple-value-bind (l tp r b) (ct-win-rect hw) (declare (ignore tp b)) (- r l)))
(defun ct-h (hw) (multiple-value-bind (l tp r b) (ct-win-rect hw) (declare (ignore l r)) (- b tp)))

(defun ct-client (hw)
  "HW's client area's size."
  (let ((r (rect-area)))
    (c *get-client-rect* hw r)
    (values (s32 (peek32 (+ r 8))) (s32 (peek32 (+ r 12))))))

(defun ct-client-h (hw) (multiple-value-bind (cw ch) (ct-client hw) (declare (ignore cw)) ch))

(defun ct-place (hw x y)
  "HW moved to X Y in its parent's client area, its size kept."
  (c *set-window-pos* hw 0 x y 0 0 #x15))                                                 ; NOSIZE|NOZORDER|NOACTIVATE

(defun ct-size (hw x y w h)
  (c *set-window-pos* hw 0 x y w h #x14))                                                 ; NOZORDER|NOACTIVATE

(defun ct-rel (hw parent)
  "HW's top left in PARENT's client area."
  (multiple-value-bind (l tp) (ct-win-rect hw)
    (let ((pt (point-area)))
      (poke32 pt (logand l #xFFFFFFFF))
      (poke32 (+ pt 4) (logand tp #xFFFFFFFF))
      (c *ct-screen-to-client* parent pt)
      (values (s32 (peek32 pt)) (s32 (peek32 (+ pt 4)))))))

(defun ct-inside (hw x y)
  (multiple-value-bind (l tp r b) (ct-win-rect hw)
    (and (<= l x) (< x r) (<= tp y) (< y b))))

;;; ---- a handle's properties ----------------------------------------------------------------------------------------------

(defun ct-prop (h key) (getf (ctl-data (ctl-of h)) key))
(defun ct-set (h key v) (setf (getf (ctl-data (ctl-of h)) key) v))
(defun ct-live (hs)
  "The handles in HS that still exist (ui_remove takes one away)."
  (remove-if-not (lambda (h) (let ((ctl (gethash h *ctls*))) (and ctl (plusp (ctl-hwnd ctl))))) hs))
(defun ct-kids-live (kids) (remove-if-not (lambda (k) (gethash (first k) *ctls*)) kids))

(defun ct-kind-of (hwnd kind)
  "The handle of the window HWND, if it is one of KIND."
  (let* ((h (gethash hwnd *by-hwnd*)) (ctl (and h (gethash h *ctls*))))
    (and ctl (eq (ctl-kind ctl) kind) h)))

(defun ct-add-child (parent child)
  "CHILD's window reparented into PARENT's window, and CHILD added to PARENT's :children (in order).  True if it was
   a window."
  (let ((hw (ctl-hwnd (ctl-of child))))
    (unless (zerop hw)
      (u *set-parent* hw (ctl-hwnd (ctl-of parent)))
      (ct-set parent :children (append (remove child (ct-prop parent :children)) (list child))))
    (not (zerop hw))))

;;; ---- text ---------------------------------------------------------------------------------------------------------

(defun ct-text-later (hw text)
  "HW's text, set on the windows' thread without waiting (as SET-TEXT does)."
  (unless (zerop hw)
    (ui-post *set-window-text* (list hw (lambda (mem) (wide-at mem text))) (lambda (r mem) r mem)
             (* 4 (+ 1 (length text))))))

(defun ct-label-text (h text) (ct-text-later (ct-prop h :label) text))

;;; ---- scroll bars ----------------------------------------------------------------------------------------------------

(defun ct-scroll-bar (hw bar lo hi page pos)
  "HW's scroll bar BAR (0 across, 1 down): its range LO..HI, its thumb PAGE long, at POS."
  (let ((si (area 5)))
    (poke32 si 28)
    (poke32 (+ si 4) 7)                                                                   ; SIF_RANGE|SIF_PAGE|SIF_POS
    (poke32 (+ si 8) (logand lo #xFFFFFFFF))
    (poke32 (+ si 12) (logand hi #xFFFFFFFF))
    (poke32 (+ si 16) (max 1 page))
    (poke32 (+ si 20) (logand pos #xFFFFFFFF))
    (c *ct-set-scroll-info* hw bar si 1)))

(defun ct-track-pos (hw bar)
  "Where the thumb of HW's bar BAR is being dragged to (SIF_TRACKPOS)."
  (let ((si (area 5)))
    (poke32 si 28)
    (poke32 (+ si 4) #x10)
    (c *ct-get-scroll-info* hw bar si)
    (s32 (peek32 (+ si 24)))))

;;; ---- stacks -------------------------------------------------------------------------------------------------------

(defun ct-stack-layout (h)
  "The children laid out in a column (or a row), GAP pixels apart, each keeping its size."
  (let ((column (ct-prop h :column)) (gap (ct-prop h :gap)) (pos 0))
    (dolist (ch (ct-live (ct-prop h :children)))
      (let ((hw (ctl-hwnd (ctl-of ch))))
        (if column
            (progn (ct-place hw 0 pos) (incf pos (+ (ct-h hw) gap)))
            (progn (ct-place hw pos 0) (incf pos (+ (ct-w hw) gap))))))))

(defun ct-stack-add (parent child)
  (when (ct-add-child parent child) (ct-stack-layout parent)))

;;; ---- scroll views ---------------------------------------------------------------------------------------------------
;;; The children are stacked down the view's content, from :offset (pixels); the bar's range is the content.
;;; Approximation: a child's content-space position is its own, the view moves them; no ScrollWindowEx.

(defun ct-scroll-layout (h)
  (let* ((hw (ctl-hwnd (ctl-of h)))
         (kids (ct-live (ct-prop h :children)))
         (gap (px *ct-scroll-gap*))
         (total 0))
    (dolist (k kids) (incf total (+ (ct-h (ctl-hwnd (ctl-of k))) gap)))
    (setf total (max 0 (- total gap)))
    (let* ((vh (ct-client-h hw))
           (offset (min (max 0 (- total vh)) (max 0 (or (ct-prop h :offset) 0))))
           (pos 0))
      (ct-set h :offset offset)
      (dolist (k kids)
        (let ((cw (ctl-hwnd (ctl-of k))))
          (ct-place cw 0 (- pos offset))
          (incf pos (+ (ct-h cw) gap))))
      (ct-scroll-bar hw 1 0 (max 0 (1- total)) vh offset))))

(defun ct-scroll-add (parent child)
  (when (ct-add-child parent child) (ct-scroll-layout parent)))

(defun ct-scroll-message (h wp)
  "A WM_VSCROLL to the scroll view H: its bar's code WP's low word."
  (let* ((hw (ctl-hwnd (ctl-of h))) (off (or (ct-prop h :offset) 0)) (vh (ct-client-h hw)))
    (ct-set h :offset
            (case (lo16 wp)
              (0 (- off (px 20)))                                                         ; SB_LINEUP
              (1 (+ off (px 20)))                                                         ; SB_LINEDOWN
              (2 (- off vh))                                                              ; SB_PAGEUP
              (3 (+ off vh))                                                              ; SB_PAGEDOWN
              ((4 5) (ct-track-pos hw 1))                                                 ; THUMBPOSITION, THUMBTRACK
              (6 0)                                                                       ; SB_TOP
              (7 100000000)                                                               ; SB_BOTTOM
              (t off)))
    (ct-scroll-layout h)))

(defswift "ui_scroll_new" (x y w h)
  (let ((hd (new-handle (make-ctl :kind :scroll
                                  :hwnd (ct-window *hwnd* (ct-layout-class) "" (logior *ct-layout-style* #x00200000)
                                                   (px x) (px y) (px w) (px h))   ; WS_VSCROLL
                                  :data (list :offset 0 :children nil)))))
    (ct-scroll-layout hd)
    hd))

;;; ---- paging -------------------------------------------------------------------------------------------------------
;;; Each child keeps the x it was added at (its base, in the view's space).  Page k shows the children offset by k
;;; page widths.  The view's horizontal bar changes the page (a :page event); ui_set_value sets it silently.

(defun ct-paging-layout (h)
  (let* ((hw (ctl-hwnd (ctl-of h))) (page (ct-prop h :page)) (w (ct-prop h :width)) (n (ct-prop h :pages)))
    (dolist (k (ct-kids-live (ct-prop h :kids)))
      (ct-place (ctl-hwnd (ctl-of (first k))) (- (second k) (* page w)) (third k)))
    (ct-scroll-bar hw 0 0 (max 0 (1- n)) 1 page)))

(defun ct-paging-add (parent child)
  (let ((hw (ctl-hwnd (ctl-of child))) (ph (ctl-hwnd (ctl-of parent))))
    (unless (zerop hw)
      (u *set-parent* hw ph)
      (multiple-value-bind (bx by) (ct-rel hw ph)
        (ct-set parent :kids (append (remove child (ct-prop parent :kids) :key #'first)
                                     (list (list child bx by)))))
      (ct-paging-layout parent))))

(defun ct-paging-set (h v)
  (ct-set h :page (min (max 0 v) (max 0 (1- (ct-prop h :pages)))))
  (ct-paging-layout h))

(defun ct-paging-scroll (h wp)
  "A WM_HSCROLL to the paging view H: the bar's code WP's low word moves the page; a page that changed is an event."
  (let* ((hw (ctl-hwnd (ctl-of h))) (old (ct-prop h :page)) (n (ct-prop h :pages))
         (new (case (lo16 wp)
                ((0 2) (1- old))
                ((1 3) (1+ old))
                ((4 5) (ct-track-pos hw 0))
                (6 0)
                (7 (1- n))
                (t old))))
    (setf new (min (max 0 new) (max 0 (1- n))))
    (unless (= new old)
      (ct-set h :page new)
      (ct-paging-layout h)
      (ui-event (list :page h new)))))

(defswift "ui_paging_new" (pages x y w h)
  (let ((hd (new-handle (make-ctl :kind :paging
                                  :hwnd (ct-window *hwnd* (ct-layout-class) "" (logior *ct-layout-style* #x00100000)
                                                   (px x) (px y) (px w) (px h))   ; WS_HSCROLL
                                  :data (list :pages (max 1 pages) :page 0 :width (px w) :kids nil)))))
    (ct-paging-layout hd)
    hd))

;;; ---- zoom scroll ----------------------------------------------------------------------------------------------------
;;; The content view is a window as large as the zoom view at 100%, and each child added to it keeps its size and
;;; place relative to it, scaled by the zoom.  Approximation: the content is not scrolled; what is beyond the
;;; zoom view's edges is clipped.  Ctrl+wheel over the view zooms it in 10% steps (100 to 400); each step is a :zoom event.

(defun ct-zoom-apply (sh)
  (let ((content (ct-prop sh :content)) (z (ct-prop sh :zoom)))
    (when content
      (flet ((s (v) (round (* v z) 100)))
        (ct-size (ctl-hwnd (ctl-of content)) 0 0 (s (ct-prop content :w)) (s (ct-prop content :h)))
        (dolist (k (ct-kids-live (ct-prop content :kids)))
          (destructuring-bind (ch bx by bw bh) k
            (ct-size (ctl-hwnd (ctl-of ch)) (s bx) (s by) (s bw) (s bh))))))))

(defun ct-zoom-set (h v)
  (ct-set h :zoom (min 400 (max 100 v)))
  (ct-zoom-apply h))

(defun ct-zoom-step (h delta)
  (let* ((old (ct-prop h :zoom)) (new (min 400 (max 100 (+ old (round (* delta 10) 120))))))
    (unless (= new old)
      (ct-zoom-set h new)
      (ui-event (list :zoom h new)))))

(defun ct-zoom-add (content child)
  (let* ((ch (ctl-hwnd (ctl-of child))) (cw (ctl-hwnd (ctl-of content)))
         (z (ct-prop (ct-prop content :scroll) :zoom)))
    (unless (zerop ch)
      (u *set-parent* ch cw)
      (multiple-value-bind (bx by) (ct-rel ch cw)
        (ct-set content :kids
                (append (remove child (ct-prop content :kids) :key #'first)
                        (list (list child (round (* bx 100) z) (round (* by 100) z)
                                    (round (* (ct-w ch) 100) z) (round (* (ct-h ch) 100) z))))))
      (ct-zoom-apply (ct-prop content :scroll)))))

(defun ct-zoom-content (sh)
  (or (ct-prop sh :content)
      (multiple-value-bind (cw ch) (ct-client (ctl-hwnd (ctl-of sh)))
        (let* ((hw (ct-window (ctl-hwnd (ctl-of sh)) (ct-layout-class) "" *ct-layout-style* 0 0 cw ch))
               (content (new-handle (make-ctl :kind :zoom-content :hwnd hw
                                              :data (list :kids nil :w cw :h ch :scroll sh)))))
          (ct-set sh :content content)
          content))))

(defswift "ui_zoom_scroll_new" (x y w h)
  (new-handle (make-ctl :kind :zoom-scroll
                        :hwnd (ct-window *hwnd* (ct-layout-class) "" *ct-layout-style* (px x) (px y) (px w) (px h))
                        :data (list :zoom 100 :content nil))))

(defswift "ui_zoom_content" (scroll) (ct-zoom-content scroll))

;;; ---- the wheel ------------------------------------------------------------------------------------------------------

(defun ct-wheel (wp lp)
  "A WM_MOUSEWHEEL: over a scroll view, it scrolls (Ctrl+wheel over a zoom view zooms it); else it is left to the main
   window.  Taken -> true."
  (let ((x (s16 lp)) (y (s16 (ash lp -16))) (delta (s16 (ash wp -16))) (ctrl (logtest wp 8)) (target nil))
    (maphash (lambda (h ctl)
               (when (and (null target)
                          (member (ctl-kind ctl) '(:scroll :zoom-scroll))
                          (plusp (ctl-hwnd ctl))
                          (ct-inside (ctl-hwnd ctl) x y))
                 (setf target h)))
             *ctls*)
    (when target
      (let ((kind (ctl-kind (ctl-of target))))
        (cond ((and (eq kind :zoom-scroll) ctrl) (ct-zoom-step target delta) t)
              ((and (eq kind :scroll) (not ctrl))
               (ct-set target :offset (- (or (ct-prop target :offset) 0) (round (* delta (px 40)) 120)))
               (ct-scroll-layout target)
               t)
              (t nil))))))

;;; ---- the crown (a trackbar and its value) ---------------------------------------------------------------------------

(defvar *ct-crown-tracks* (make-hash-table) "A crown's trackbar window -> the crown's handle.")

(defun ct-crown-set (h v)
  (let ((v (min 100 (max 0 v))))
    (ct-set h :value v)
    (c *send-message* (ct-prop h :track) #x405 1 v)                                       ; TBM_SETPOS
    (ct-text-later (ct-prop h :value-label) (format nil "~D" v))))

(defun ct-crown-moved (h)
  "The trackbar moved: its new position is a :crown event, if the value changed."
  (let ((v (logand (c *send-message* (ct-prop h :track) #x400 0 0) #xFFFFFFFF)))           ; TBM_GETPOS
    (unless (eql v (ct-prop h :value))
      (ct-set h :value v)
      (ct-text-later (ct-prop h :value-label) (format nil "~D" v))
      (ui-event (list :crown h v)))))

(defun ct-gauge-set (h v)
  (ct-set h :value v)
  (c *send-message* (ct-prop h :bar) #x402 v 0))                                          ; PBM_SETPOS

;;; ---- the message hook -----------------------------------------------------------------------------------------------

(defun ct-hook (msg hwnd wp lp)
  (cond ((= msg #x115)                                                                    ; WM_VSCROLL
         (let ((h (ct-kind-of hwnd :scroll)))
           (when h (ct-scroll-message h wp) t)))
        ((= msg #x114)                                                                    ; WM_HSCROLL
         (let ((crown (gethash lp *ct-crown-tracks*))
               (paging (and (zerop lp) (ct-kind-of hwnd :paging))))
           (cond ((and crown (gethash crown *ctls*)) (ct-crown-moved crown) t)
                 (paging (ct-paging-scroll paging wp) t)
                 (t nil))))
        ((= msg #x20A) (ct-wheel wp lp))                                                  ; WM_MOUSEWHEEL
        (t nil)))

(pushnew #'ct-hook *message-hooks*)

(setf (gethash :stack *add-handlers*) #'ct-stack-add
      (gethash :scroll *add-handlers*) #'ct-scroll-add
      (gethash :paging *add-handlers*) #'ct-paging-add
      (gethash :zoom-content *add-handlers*) #'ct-zoom-add)

(setf (gethash :gauge *set-value-handlers*) #'ct-gauge-set
      (gethash :crown *set-value-handlers*) #'ct-crown-set
      (gethash :paging *set-value-handlers*) #'ct-paging-set
      (gethash :zoom-scroll *set-value-handlers*) #'ct-zoom-set)

(setf (gethash :gauge *set-text-handlers*) #'ct-label-text
      (gethash :crown *set-text-handlers*) #'ct-label-text
      (gethash :unavailable *set-text-handlers*) #'ct-label-text)

;;; ---- filled views and shapes ----------------------------------------------------------------------------------------

(defswift "ui_panel_new" (x y w h rgb)
  (let ((cls (ct-fill-class rgb)))
    (if (zerop w)
        (multiple-value-bind (cw ch) (ct-client *hwnd*)                                   ; w 0: the whole window
          (new-handle (make-ctl :kind :panel :hwnd (ct-window *hwnd* cls "" *ct-layout-style* 0 0 cw ch))))
        (new-handle (make-ctl :kind :panel
                              :hwnd (ct-window *hwnd* cls "" *ct-layout-style* (px x) (px y) (px w) (px h)))))))

(defswift "ui_blur_new" (x y w h)
  ;; Approximation: no blur of what is behind; a dark translucent-looking fill.  Children go on top (default ui_add).
  (new-handle (make-ctl :kind :blur
                        :hwnd (ct-window *hwnd* (ct-fill-class #x1C2430) "" *ct-layout-style*
                                         (px x) (px y) (px w) (px h)))))

(defun ct-shape (kind x y w h rgb)
  "A filled shape of W x H points: KIND 0 an oval, 1 a rounded rectangle (corners 22% of the short side), in RGB."
  (let* ((pw (px w)) (ph (px h))
         (diam (round (* 44 (min pw ph)) 100))
         (hw (ct-window *hwnd* (ct-fill-class rgb) "" *ct-layout-style* (px x) (px y) pw ph))
         (rgn (if (eql kind 0)
                  (c *ct-create-elliptic-rgn* 0 0 (+ pw 1) (+ ph 1))
                  (c *create-round-rgn* 0 0 (+ pw 1) (+ ph 1) diam diam))))
    (u *set-window-rgn* hw rgn 1)
    (new-handle (make-ctl :kind :shape :hwnd hw))))

(defswift "ui_shape_new" (kind x y w h rgb) (ct-shape kind x y w h rgb))

(defswift "ui_drawn_image_new" (kind rgb size x y) (ct-shape kind x y size size rgb))

;; Approximation: the PNG file the Mac host saves is not written; the shape is drawn directly.
(defswift "ui_file_image_new" (kind rgb size x y) (ct-shape kind x y size size rgb))

;;; ---- stack --------------------------------------------------------------------------------------------------------

(defswift "ui_stack_new" (vertical spacing x y w h)
  ;; VERTICAL 0 is a column (children stacked down), 1 a row (across).
  (new-handle (make-ctl :kind :stack
                        :hwnd (ct-window *hwnd* (ct-layout-class) "" *ct-layout-style* (px x) (px y) (px w) (px h))
                        :data (list :column (zerop vertical) :gap (px spacing) :children nil))))

;;; ---- images and labels ----------------------------------------------------------------------------------------------

(defswift "ui_image_new" (a n x y w h)
  ;; Approximation: a glyph of Segoe Fluent Icons for the names in *CT-GLYPHS*; any other name is shown as its text.
  (let* ((name (utf8-text a n))
         (glyph (cdr (assoc name *ct-glyphs* :test #'string=)))
         (hw (ct-window *hwnd* "STATIC" (if glyph (string (code-char glyph)) name) #x81   ; SS_CENTER|SS_NOPREFIX
                        (px x) (px y) (px w) (px h))))
    (when glyph
      (ct-set-font hw (ct-font (max 8 (floor (* 3 (min w h)) 5)) 400 0 "Segoe Fluent Icons")))
    (new-handle (make-ctl :kind :image :hwnd hw))))

(defswift "ui_unavailable_new" (a n x y w h)
  ;; An empty-state view: a warning glyph above a centred text.
  (let* ((pw (px w)) (ph (px h))
         (hw (ct-window *hwnd* (ct-layout-class) "" *ct-layout-style* (px x) (px y) pw ph))
         (icon (ct-window hw "STATIC" (string (code-char #xE7BA)) #x81 0 0 pw (floor ph 3)))
         (label (ct-window hw "STATIC" (utf8-text a n) #x81 0 (floor ph 2) pw (floor ph 3))))
    (ct-set-font icon (ct-font (max 8 (floor (* 2 (min w h)) 5)) 400 0 "Segoe Fluent Icons"))
    (new-handle (make-ctl :kind :unavailable :hwnd hw :data (list :label label)))))

(defswift "ui_attributed_label_new" (a n x y w h)
  ;; The text underlined; the tint is the window's text colour.
  (let ((hw (ct-window *hwnd* "STATIC" (utf8-text a n) #x80 (px x) (px y) (px w) (px h))))
    (ct-set-font hw (ct-font 14 400 1 "Segoe UI"))
    (new-handle (make-ctl :kind :attributed :hwnd hw))))

(defswift "ui_dynamic_label_new" (a n x y w h)
  ;; Approximation: the body style at a fixed 17 points (no Dynamic Type on Windows).
  (let ((hw (ct-window *hwnd* "STATIC" (utf8-text a n) #x80 (px x) (px y) (px w) (px h))))
    (ct-set-font hw (ct-font 17 400 0 "Segoe UI"))
    (new-handle (make-ctl :kind :dynamic :hwnd hw))))

;;; ---- gauge and crown ------------------------------------------------------------------------------------------------

(defswift "ui_gauge_new" (value x y w h)
  ;; A progress bar at the foot of a window, its label above it (set by ui_set_text).
  (let* ((pw (px w)) (ph (px h)) (bh (max 6 (floor ph 4)))
         (hw (ct-window *hwnd* (ct-layout-class) "" *ct-layout-style* (px x) (px y) pw ph))
         (label (ct-window hw "STATIC" "" #x80 0 0 pw (- ph bh)))
         (bar (ct-window hw "msctls_progress32" "" 0 0 (- ph bh) pw bh)))
    (c *send-message* bar #x401 0 (ash 100 16))                                           ; PBM_SETRANGE 0..100
    (c *send-message* bar #x402 value 0)                                                  ; PBM_SETPOS
    (new-handle (make-ctl :kind :gauge :hwnd hw :data (list :bar bar :label label :value value)))))

(defswift "ui_crown_new" (a n x y w h)
  ;; The label on the left, the value at its right, the trackbar under them.  Each move is a :crown event.
  (let* ((pw (px w)) (ph (px h)) (row (floor ph 2)) (lw (floor (* pw 3) 5))
         (hw (ct-window *hwnd* (ct-layout-class) "" *ct-layout-style* (px x) (px y) pw ph))
         (label (ct-window hw "STATIC" (utf8-text a n) #x80 0 0 lw row))
         (value (ct-window hw "STATIC" "0" #x82 lw 0 (- pw lw) row))                      ; SS_RIGHT
         (track (ct-window hw "msctls_trackbar32" "" #x10 0 row pw (- ph row))))          ; TBS_NOTICKS
    (c *send-message* track #x406 1 (ash 100 16))                                         ; TBM_SETRANGE 0..100
    (c *send-message* track #x405 1 0)                                                    ; TBM_SETPOS
    (let ((hd (new-handle (make-ctl :kind :crown :hwnd hw
                                    :data (list :track track :label label :value-label value :value 0)))))
      (setf (gethash track *ct-crown-tracks*) hd)
      hd)))

;;; ---- the screen -------------------------------------------------------------------------------------------------------

(defswift "ui_screen_width" ()
  ;; The window's client width, in points.
  (client-size)
  (round (* *w* 96) *dpi*))

(defswift "ui_screen_height" ()
  (client-size)
  (round (* *h* 96) *dpi*))

(defswift "ui_safe_area_top" () 0)
