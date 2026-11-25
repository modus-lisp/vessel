(in-package #:vessel-windows)

;;;; win-ui-nav.lisp -- navigation, sheets and system pickers for the Windows host (win-host.lisp).
;;;;
;;;; A sheet is a hidden top-level window owned by the app's window, shown centred over it (or beside a source
;;;; window for a popover): ui_modal_new, ui_nav_controller_new, ui_tab_controller_new, ui_page_controller_new and
;;;; ui_split_present make sheets.  A sheet has its own window procedure page (as window_new does), so its close
;;;; box arrives as WM_CLOSE on the ring and is answered as (:dismiss h), not destroyed by DefWindowProc.
;;;; A nav bar is a panel with a back button, and per item a title label and an optional right button.
;;;; The share sheet is the clipboard, answered as (:share h 1|0).  The colour and font pickers are ChooseColorW and
;;;; ChooseFontW, run through UI-POST so they never block the app; the image picker is the file dialog (CHOOSE).

;;; ---- the Win32 calls this file adds to win-host's DEFPROCS ---------------------------------------------------------

(defvar *nv-get-window-rect* (modus-int::w32-proc "user32.dll" "GetWindowRect"))
(defvar *nv-get-window* (modus-int::w32-proc "user32.dll" "GetWindow"))
(defvar *nv-choose-color* (modus-int::w32-proc "comdlg32.dll" "ChooseColorW"))
(defvar *nv-choose-font* (modus-int::w32-proc "comdlg32.dll" "ChooseFontW"))

;;; WS_POPUP | WS_CLIPCHILDREN | WS_CAPTION | WS_SYSMENU: an owned window with a caption and a close box.
(defparameter *nv-style* #x82C80000)
(defparameter *nv-bar-h* 44 "A navigation bar's height, in points.")
(defparameter *nv-sheet-count* 0)
(defvar *nv-clicks* (make-hash-table) "A button's hwnd -> the function a click on it calls.")

;;; ---- windows, without the handles ---------------------------------------------------------------------------------

(defun nv-text (a n) (if (plusp n) (utf8-text a n) ""))

(defun nv-make (parent class text style x y w h)
  "A child window of PARENT (an hwnd), made on the windows' thread: CLASS, TEXT, at X Y W H points."
  (let ((hwnd (u *create-window* 0 (wide1 class) (wide2 text) (logior #x50000000 style)   ; WS_CHILD|WS_VISIBLE
                 (px x) (px y) (px w) (px h) parent 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no ~A" class))
    (c *send-message* hwnd #x30 *font* 1)                                                 ; WM_SETFONT
    hwnd))

(defun nv-panel (parent x y w h)
  "A panel (a VesselPanel, in the panel colour) of PARENT at X Y W H points."
  (let ((hwnd (u *create-window* 0 (wide1 "VesselPanel") (wide2 "") #x56000000    ; CHILD VISIBLE CLIP*
                 (px x) (px y) (px w) (px h) parent 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no panel"))
    hwnd))

(defun nv-place (hwnd x y w h)
  "HWND at X Y, W x H points, without changing the stacking order or activating anything."
  (u *set-window-pos* hwnd 0 (px x) (px y) (px w) (px h) #x14))                          ; NOZORDER | NOACTIVATE

(defun nv-show (hwnd on)
  (unless (zerop hwnd) (u *show-window* hwnd (if on 5 0))))

(defun nv-screen-rect (hwnd at)
  "HWND's rectangle on the screen, read into the memory AT: (values left top right bottom)."
  (c *nv-get-window-rect* hwnd at)
  (values (s32 (peek32 at)) (s32 (peek32 (+ at 4))) (s32 (peek32 (+ at 8))) (s32 (peek32 (+ at 12)))))

(defun nv-children (hwnd)
  "HWND's child windows, in their stacking order."
  (let ((kids '()) (k (logand (c *nv-get-window* hwnd 5) #xFFFFFFFF)))     ; GW_CHILD
    (loop until (zerop k)
          do (push k kids)
             (setf k (logand (c *nv-get-window* k 2) #xFFFFFFFF)))          ; GW_HWNDNEXT
    (nreverse kids)))

;;; ---- sheets -------------------------------------------------------------------------------------------------------

(defun nv-sheet-new (w h)
  "A sheet of W x H points: a hidden top-level window owned by the app's window, with a window procedure page of its own
   (window_new's way), so its close box is an event."
  (let* ((page (make-page))
         (class (format nil "VesselSheet~D" (incf *nv-sheet-count*)))
         (r (rect-area)))
    (dolist (off '(#x40 #x48 #x50 #x58)) (poke64 (+ page off) (peek64 (+ *page* off))))
    (poke64 (+ page #x40) *arrow*)
    (register-class class *arrow* (peek64 (+ *page* #x58)) page)
    (zero r 16)
    (poke32 (+ r 8) (px w)) (poke32 (+ r 12) (px h))
    (c *adjust-rect* r *nv-style* 0 0)
    (let ((hwnd (u *create-window* 0 (wide1 class) (wide2 "") *nv-style*
                   #x80000000 #x80000000
                   (- (s32 (peek32 (+ r 8))) (s32 (peek32 r)))
                   (- (s32 (peek32 (+ r 12))) (s32 (peek32 (+ r 4))))
                   *hwnd* 0 *hinst* 0)))
      (when (zerop hwnd) (error "vessel-windows: no sheet"))
      (theme-frame hwnd)
      (poke64 (+ page #x10) hwnd)
      (setf *pages* (append *pages* (list page)))
      (new-handle (make-ctl :kind :sheet :hwnd hwnd :data (list :w w :h h))))))

(defun nv-sheet-place (hwnd w h source)
  "Size the sheet HWND to W x H points: centred over the app's window or, with SOURCE (a window), just right of it."
  (let ((r (rect-area)))
    (zero r 16)
    (poke32 (+ r 8) (px w)) (poke32 (+ r 12) (px h))
    (c *adjust-rect* r *nv-style* 0 0)
    (let* ((ow (- (s32 (peek32 (+ r 8))) (s32 (peek32 r))))
           (oh (- (s32 (peek32 (+ r 12))) (s32 (peek32 (+ r 4)))))
           (x 0) (y 0))
      (multiple-value-bind (ml mt mr mb) (nv-screen-rect *hwnd* (+ *cb* #x5140))
        (setf x (+ ml (floor (- (- mr ml) ow) 2))
              y (+ mt (floor (- (- mb mt) oh) 2))))
      (unless (zerop source)
        (multiple-value-bind (sl st sr sb) (nv-screen-rect source (+ *cb* #x5160))
          (declare (ignore sl sb))
          (setf x (+ sr 8) y st)))
      (u *set-window-pos* hwnd 0 x y ow oh #x10))))                                       ; NOACTIVATE

(defun nv-sheet-show (h w ht &optional (source 0))
  "Show the sheet H at W x HT points, centred over the app's window (or beside SOURCE's window). 0."
  (let* ((ctl (ctl-of h)) (hwnd (ctl-hwnd ctl)))
    (setf (getf (ctl-data ctl) :w) w (getf (ctl-data ctl) :h) ht)
    (when (ctl-value ctl) (funcall (ctl-value ctl) w ht))
    (nv-sheet-place hwnd w ht source)
    (u *show-window* hwnd 5)
    0))

(defun nv-sheet-closed (h)
  "The close box of the sheet H: hidden, and a :dismiss event."
  (u *show-window* (ctl-hwnd (ctl-of h)) 0)
  (ui-event (list :dismiss h)))

(defun nv-message-hook (msg hwnd wp lp)
  "A click on one of this file's buttons calls its function; the close box of a sheet hides it (:dismiss)."
  (cond ((and (= msg #x111) (zerop (hi16 wp)) (gethash lp *nv-clicks*))   ; WM_COMMAND, BN_CLICKED
         (funcall (gethash lp *nv-clicks*))
         t)
        ((= msg #x10)                                                    ; WM_CLOSE
         (let ((h (gethash hwnd *by-hwnd*)))
           (when (and h (eq (ctl-kind (ctl-of h)) :sheet))
             (nv-sheet-closed h)
             t)))
        (t nil)))

(pushnew 'nv-message-hook *message-hooks*)

;;; ---- navigation bars ----------------------------------------------------------------------------------------------
;;;
;;; ITEMS: the stack, top first; each (TITLE-LABEL RIGHT-BUTTON VIEW) as hwnds (0: none).  Only the top item's controls
;;; show; the back button shows when there is more than one.

(defstruct nvbar (hwnd 0) (w 0) (h 0) (items '()) (back 0) (handle 0))

(defun nv-bar-layout (bar)
  (let ((w (nvbar-w bar)) (h (nvbar-h bar)))
    (nv-place (nvbar-back bar) 4 4 80 (- h 8))
    (dolist (item (nvbar-items bar))
      (destructuring-bind (title right view) item
        (declare (ignore view))
        (nv-place title 88 4 (- w (if (zerop right) 96 176)) (- h 8))
        (unless (zerop right) (nv-place right (- w 84) 4 80 (- h 8)))))))

(defun nv-bar-show-top (bar)
  (loop for item in (nvbar-items bar)
        for top = t then nil
        do (destructuring-bind (title right view) item
             (nv-show title top) (nv-show right top) (nv-show view top)))
  (nv-show (nvbar-back bar) (rest (nvbar-items bar))))

(defun nv-bar-push (bar title right view)
  "Push an item titled TITLE, with a right button labelled RIGHT (NIL: none) and the view VIEW (0: none). 0."
  (let* ((parent (nvbar-hwnd bar))
         (label (nv-make parent "STATIC" title #x81 0 0 1 1))                  ; SS_CENTER | SS_NOPREFIX
         (button (if right (nv-make parent "BUTTON" right 0 0 0 1 1) 0))
         (handle (nvbar-handle bar)))
    (unless (zerop button)
      (setf (gethash button *nv-clicks*) (lambda () (ui-event (list :navtap handle)))))
    (push (list label button view) (nvbar-items bar))
    (nv-bar-layout bar)
    (nv-bar-show-top bar)
    0))

(defun nv-bar-pop (bar)
  "Pop the top item, hiding it and its view; the item under it shows. NIL if there is nothing to pop to."
  (let ((items (nvbar-items bar)))
    (when (rest items)
      (destructuring-bind (title right view) (first items)
        (nv-show title nil) (nv-show right nil) (nv-show view nil))
      (setf (nvbar-items bar) (rest items))
      (nv-bar-layout bar)
      (nv-bar-show-top bar)
      t)))

(defun nv-bar-new (parent x y w h on-back)
  "A navigation bar: a panel of PARENT at X Y W H points, with its back button. ON-BACK is called with the bar when
   the back button is tapped."
  (let* ((panel (nv-panel parent x y w h))
         (bar (make-nvbar :hwnd panel :w w :h h))
         (back (nv-make panel "BUTTON" "back" 0 0 0 1 1)))
    (setf (nvbar-back bar) back)
    (setf (gethash back *nv-clicks*) (lambda () (funcall on-back bar)))
    (nv-show back nil)
    (setf (nvbar-handle bar) (new-handle (make-ctl :kind :navbar :hwnd panel :data bar)))
    bar))

(defun nv-bar-relayout (bar w ht)
  "The bar and its views for a sheet of W x HT points."
  (setf (nvbar-w bar) w (nvbar-h bar) *nv-bar-h*)
  (nv-place (nvbar-hwnd bar) 0 0 w *nv-bar-h*)
  (nv-bar-layout bar)
  (dolist (item (nvbar-items bar))
    (unless (zerop (third item))
      (nv-place (third item) 0 *nv-bar-h* w (- ht *nv-bar-h*)))))

;;; ---- tabs and pages -----------------------------------------------------------------------------------------------

(defun nv-tab-layout (ctl w ht)
  (let* ((d (ctl-data ctl))
         (buttons (getf d :buttons))
         (tw (floor w (max 1 (length buttons)))))
    (loop for b in buttons for i from 0
          do (nv-place b (* i tw) 0 tw 36))
    (dolist (p (getf d :panels)) (nv-place p 0 36 w (- ht 36)))))

(defun nv-tabs-show (ctl index)
  (loop for p in (getf (ctl-data ctl) :panels) for k from 0
        do (nv-show p (= k index)))
  (setf (getf (ctl-data ctl) :current) index))

(defun nv-tabs-select (h ctl index)
  "A tab was chosen: its view shows, and (:select h index)."
  (nv-tabs-show ctl index)
  (ui-event (list :select h index)))

(defun nv-pager-layout (ctl w ht)
  (let ((d (ctl-data ctl)))
    (dolist (p (getf d :panels)) (nv-place p 0 0 w (- ht 44)))
    (nv-place (getf d :prev) 8 (- ht 40) 100 32)
    (nv-place (getf d :next) (- w 108) (- ht 40) 100 32)))

(defun nv-pager-show (ctl index)
  (loop for p in (getf (ctl-data ctl) :panels) for k from 0
        do (nv-show p (= k index)))
  (setf (getf (ctl-data ctl) :current) index))

;;; ---- the pickers' answers -----------------------------------------------------------------------------------------

(defun nv-color-answer (h r mem)
  "ChooseColorW's answer, with its struct at MEM: a colour picked is (:color h rgb), rgb 0xRRGGBB from the COLORREF
   0x00BBGGRR in rgbResult."
  (unless (zerop (logand r #xFFFFFFFF))
    (let ((cr (peek32 (+ mem 24))))
      (ui-event (list :color h (logior (ash (ldb (byte 8 0) cr) 16)
                                       (ash (ldb (byte 8 8) cr) 8)
                                       (ldb (byte 8 16) cr)))))))

(defun nv-font-answer (h r mem)
  "ChooseFontW's answer, with its struct at MEM and its LOGFONTW at MEM+104: a font picked is (:font h face)."
  (unless (zerop (logand r #xFFFFFFFF))
    (ui-event (list :font h (or (modus-int::w32-wstr (+ mem 132)) "")))))   ; lfFaceName at LOGFONTW+28

;;; ---- the calls ----------------------------------------------------------------------------------------------------

(defswift "ui_modal_new" () (nv-sheet-new 400 320))

(defswift "ui_modal_present" (modal large)
  (nv-sheet-show modal 400 (if (plusp large) 560 320)))

(defswift "ui_modal_dismiss" (modal)
  (u *show-window* (ctl-hwnd (ctl-of modal)) 0)
  0)

(defswift "ui_nav_bar_new" (x y w h)
  (nvbar-handle
   (nv-bar-new *hwnd* x y w h
               (lambda (bar)
                 (when (nv-bar-pop bar) (ui-event (list :pop (nvbar-handle bar))))))))

(defswift "ui_nav_push" (bar ta tn ra rn)
  (nv-bar-push (ctl-data (ctl-of bar)) (nv-text ta tn) (if (plusp rn) (utf8-text ra rn) nil) 0))

(defswift "ui_nav_pop" (bar)
  (nv-bar-pop (ctl-data (ctl-of bar)))
  0)

(defswift "ui_nav_controller_new" ()
  (let* ((h (nv-sheet-new 400 320))
         (ctl (ctl-of h))
         (bar (nv-bar-new (ctl-hwnd ctl) 0 0 400 *nv-bar-h* (lambda (b) (nv-bar-pop b))))
         (root (nv-panel (ctl-hwnd ctl) 0 *nv-bar-h* 400 (- 320 *nv-bar-h*))))
    (nv-bar-push bar "" nil root)
    (setf (getf (ctl-data ctl) :navbar) bar
          (getf (ctl-data ctl) :root) (new-handle (make-ctl :kind :panel :hwnd root))
          (ctl-value ctl) (lambda (w ht) (nv-bar-relayout bar w ht)))
    (nv-bar-relayout bar 400 320)
    h))

(defswift "ui_nav_root" (nav)
  (or (getf (ctl-data (ctl-of nav)) :root) 0))

(defswift "ui_nav_push_view" (nav a n)
  (let* ((ctl (ctl-of nav))
         (d (ctl-data ctl))
         (panel (nv-panel (ctl-hwnd ctl) 0 *nv-bar-h* (getf d :w) (- (getf d :h) *nv-bar-h*))))
    (nv-bar-push (getf d :navbar) (nv-text a n) nil panel)
    (new-handle (make-ctl :kind :panel :hwnd panel))))

(defswift "ui_tab_controller_new" (a n)
  (let* ((titles (term-arg a n))
         (h (nv-sheet-new 400 320))
         (ctl (ctl-of h))
         (hwnd (ctl-hwnd ctl))
         (buttons (loop for title in titles collect (nv-make hwnd "BUTTON" title 0 0 0 1 1)))
         (panels (loop for title in titles collect (nv-panel hwnd 0 36 400 284))))
    (loop for b in buttons for i from 0
          do (let ((i i))
               (setf (gethash b *nv-clicks*) (lambda () (nv-tabs-select h ctl i)))))
    (setf (getf (ctl-data ctl) :buttons) buttons
          (getf (ctl-data ctl) :panels) panels
          (getf (ctl-data ctl) :handles)
          (mapcar (lambda (p) (new-handle (make-ctl :kind :panel :hwnd p))) panels)
          (ctl-value ctl) (lambda (w ht) (nv-tab-layout ctl w ht)))
    (nv-tab-layout ctl 400 320)
    (nv-tabs-show ctl 0)
    h))

(defswift "ui_tab_content" (tabs index)
  (or (nth index (getf (ctl-data (ctl-of tabs)) :handles)) 0))

(defswift "ui_page_controller_new" (count)
  (let* ((n (max 1 count))
         (h (nv-sheet-new 400 320))
         (ctl (ctl-of h))
         (hwnd (ctl-hwnd ctl))
         (panels (loop repeat n collect (nv-panel hwnd 0 0 400 276)))
         (prev (nv-make hwnd "BUTTON" "previous" 0 8 280 100 32))
         (next (nv-make hwnd "BUTTON" "next" 0 292 280 100 32)))
    (flet ((turn (i)
             (when (and (>= i 0) (< i n))
               (nv-pager-show ctl i)
               (ui-event (list :page h i)))))
      (setf (gethash prev *nv-clicks*) (lambda () (turn (1- (getf (ctl-data ctl) :current))))
            (gethash next *nv-clicks*) (lambda () (turn (1+ (getf (ctl-data ctl) :current))))))
    (setf (getf (ctl-data ctl) :panels) panels
          (getf (ctl-data ctl) :prev) prev
          (getf (ctl-data ctl) :next) next
          (getf (ctl-data ctl) :current) 0
          (getf (ctl-data ctl) :handles)
          (mapcar (lambda (p) (new-handle (make-ctl :kind :panel :hwnd p))) panels)
          (ctl-value ctl) (lambda (w ht) (nv-pager-layout ctl w ht)))
    (nv-pager-layout ctl 400 320)
    (nv-pager-show ctl 0)
    h))

(defswift "ui_page_content" (pager index)
  (or (nth index (getf (ctl-data (ctl-of pager)) :handles)) 0))

(defswift "ui_popover_present" (modal source)
  (let ((d (ctl-data (ctl-of modal))))
    (nv-sheet-show modal (getf d :w) (getf d :h) (ctl-hwnd (ctl-of source)))))

(defswift "ui_split_present" (primary secondary)
  (let* ((pc (ctl-of primary)) (sc (ctl-of secondary))
         (pw (getf (ctl-data pc) :w)) (ph (getf (ctl-data pc) :h))
         (sw (getf (ctl-data sc) :w)) (sh (getf (ctl-data sc) :h))
         (h (nv-sheet-new (+ pw sw) (max ph sh)))
         (hwnd (ctl-hwnd (ctl-of h)))
         (left (nv-panel hwnd 0 0 pw ph))
         (right (nv-panel hwnd pw 0 sw sh)))
    ;; each content's child windows move into its side; the two sheets then hide
    (loop for (modal wrapper) in (list (list (ctl-hwnd pc) left) (list (ctl-hwnd sc) right))
          do (dolist (k (nv-children modal)) (u *set-parent* k wrapper))
             (u *show-window* modal 0))
    (nv-sheet-show h (+ pw sw) (max ph sh))
    h))

(defswift "ui_share_present" (a n)
  (let* ((ok (clipboard-set (nv-text a n)))
         (h (new-handle (make-ctl :kind :share))))
    (ui-event (list :share h ok))
    h))

(defswift "ui_color_picker_show" ()
  (let ((h (new-handle (make-ctl :kind :color))))
    (ui-post *nv-choose-color*
             (list (lambda (mem)
                     (zero mem 136)
                     (poke32 mem 72)                                ; lStructSize
                     (poke64 (+ mem 8) *hwnd*)                      ; hwndOwner
                     (poke64 (+ mem 32) (+ mem 72))                 ; lpCustColors: 16 COLORREFs after the struct
                     (poke32 (+ mem 40) 3)                          ; CC_RGBINIT | CC_FULLOPEN
                     mem))
             (lambda (r mem) (nv-color-answer h r mem))
             320)
    h))

(defswift "ui_font_picker_show" ()
  (let ((h (new-handle (make-ctl :kind :font))))
    (ui-post *nv-choose-font*
             (list (lambda (mem)
                     (zero mem 200)                                 ; CHOOSEFONTW, then its LOGFONTW at +104
                     (poke32 mem 104)                               ; lStructSize
                     (poke64 (+ mem 8) *hwnd*)                      ; hwndOwner
                     (poke64 (+ mem 24) (+ mem 104))                ; lpLogFont
                     (poke32 (+ mem 36) #x41)                       ; CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT
                     mem))
             (lambda (r mem) (nv-font-answer h r mem))
             320)
    h))

(defswift "ui_image_picker_show" ()
  (choose nil "Choose a Picture" :picked (new-handle (make-ctl :kind :picker)) nil
          '("Pictures" "*.png;*.jpg;*.jpeg;*.gif;*.bmp;*.webp;*.heic")))
