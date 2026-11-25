;;;; win-ui-choices.lisp -- the choices and lists of the Windows host: segmented controls, toolbars, tab bars, pickers,
;;;; menu buttons, grids, lists (their rows, row actions, deleting and refresh) and collections.  These are the calls
;;;; ios-swift.lisp makes (ui_segmented_new ... ui_collection_new), answered with Win32 controls under win-host.lisp's
;;;; window.  Loads after win-host.lisp, and uses its calls, its ring of messages and its extension points.
;;;;
;;;;   segmented control, toolbar, grid, collection  a panel of its own (the window's VesselPanel class, so the
;;;;                                                 notifications of its buttons reach the ring), with the
;;;;                                                 choices as buttons in it
;;;;   tab bar                                       a SysTabControl32
;;;;   picker                                        a LISTBOX
;;;;   list                                          a ListView in report mode, one column, no header
;;;;   menu button                                   a push button, "options", that pops a menu of its choices
;;;;
;;;; Events: (:select h index) for a choice, a tab, a picked row; (:menu h index) for a menu button's choice;
;;;; (:rowaction h index) for a list's action; (:delete h row) for a deleted row; (:refresh h) for a refresh.
;;;; A list's row menu (a right click on it) holds the actions, then Delete (when the list is editable), then Refresh
;;;; (when it can be refreshed).  A popup menu is TrackPopupMenu, which is modal, so it runs on the windows' thread
;;;; (UI-POST) and its answer comes back to a function given to CHOICE-POPUP.
;;;;
;;;; Notifications arrive as WM_COMMAND (a button's BN_CLICKED, a listbox's LBN_SELCHANGE) and WM_NOTIFY (a tab's
;;;; TCN_SELCHANGE; a list's LVN_ITEMCHANGED and NM_RCLICK), which the window's ring keeps.  A control's own keyboard
;;;; and mouse messages do not reach the ring (the control's window procedure takes them), which is why a list is a
;;;; ListView -- its right click comes as a notification -- and why a list has no key shortcuts.
;;;; CHOICE-MESSAGE is in *MESSAGE-HOOKS*, first.

(in-package #:vessel-windows)

(defvar *destroy-menu* (modus-int::w32-proc "user32.dll" "DestroyMenu"))

(defparameter *grid-tile-min* 120 "A grid tile's least width, points: the grid has as many columns as fit.")
(defparameter *tile-gap* 8 "Points between tiles, and between a collection's sections.")
(defparameter *tile-height* 72 "A grid or collection tile's height, points.")

;;; ---- handles ----------------------------------------------------------------------------------------------------
;;;
;;; A choice's handle is a CTL of its kind (:ui-segmented ...).  Its windows (a button, a list's control) map to
;;; (KIND HANDLE INDEX) in *CHOICE-HWNDS*.  A removed handle is not in *CTLS* any more, so its windows are not
;;; answered, even if Windows reuses their numbers.

(defvar *choice-hwnds* (make-hash-table) "A choice's window -> (kind handle index).")

(defstruct choice-list (rows '()) (actions '()) (editing nil) (refresh nil) (last nil))

(defun register-choice (hwnd kind h &optional (index 0))
  (setf (gethash hwnd *choice-hwnds*) (list kind h index)))

(defun live-choice (hwnd)
  "The entry of the choice window HWND, if its handle still exists."
  (let ((e (gethash hwnd *choice-hwnds*)))
    (and e (gethash (second e) *ctls*) e)))

(defun choice-window (h) (ctl-hwnd (ctl-of h)))

;;; ---- small helpers ----------------------------------------------------------------------------------------------

(defun send-to (hwnd msg &optional (wp 0) (lp 0))
  "SendMessageW to HWND on the windows' thread: its result, 32 bits."
  (logand (u *send-message* hwnd msg wp lp) #xFFFFFFFF))

(defun index-or-nil (r)
  "A result that is an index, or -1 (none): the index, else NIL."
  (let ((x (logand r #xFFFFFFFF))) (if (= x #xFFFFFFFF) nil x)))

(defun notify-code= (lp code)
  "The notification code LP (zero-extended, 32 bits) is CODE (a negative one, as the SDK defines it)."
  (= (logand lp #xFFFFFFFF) (logand code #xFFFFFFFF)))

(defun text-of (x)
  (cond ((null x) "") ((stringp x) x) (t (format nil "~A" x))))

(defun term-titles (a n)
  "The strings of the term (a list of titles) in the N bytes A names."
  (let ((v (term-arg a n))) (if (listp v) (mapcar #'text-of v) '())))

(defun row-field (row key)
  "The text of KEY in a row plist (:title, :subtitle, :section), or \"\"."
  (if (listp row) (text-of (getf row key)) ""))

(defun row-title (row) (row-field row :title))

(defun row-text (row)
  "A list row's text: its title, and its subtitle after a dash."
  (let ((title (row-title row)) (sub (row-field row :subtitle)))
    (if (plusp (length sub)) (format nil "~A ~C ~A" title (code-char #x2014) sub) title)))

(defun make-window-in (parent class text style x y w h)
  "A child window of PARENT of CLASS, with TEXT and the Win32 STYLE, at the points X Y, W by H, in the app's font."
  (let ((hwnd (u *create-window* 0 (wide1 class) (wide2 text) (logior #x50000000 style)   ; WS_CHILD | WS_VISIBLE
                 (px x) (px y) (px w) (px h) parent 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no ~A" class))
    (send-to hwnd #x30 *font* 1)                                                         ; WM_SETFONT
    hwnd))

(defun choice-container (kind x y w h)
  "A choice's container: a panel of the window's class, so its children's notifications reach the ring."
  (let ((hwnd (u *create-window* 0 (wide1 "VesselPanel") (wide2 "") #x56000000
                 (px x) (px y) (px w) (px h) *hwnd* 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no choice panel"))
    (new-handle (make-ctl :kind kind :hwnd hwnd))))

(defun window-to-screen (hwnd x y)
  "The point X Y, in HWND's client area, as screen coordinates: two values."
  (let ((p (point-area)))
    (poke32 p x) (poke32 (+ p 4) y)
    (c *map-points* hwnd 0 p 1)
    (values (s32 (peek32 p)) (s32 (peek32 (+ p 4))))))

(defun screen-to-window (hwnd x y)
  "The screen point X Y as HWND's client coordinates: two values."
  (let ((p (point-area)))
    (poke32 p x) (poke32 (+ p 4) y)
    (c *map-points* 0 hwnd p 1)
    (values (s32 (peek32 p)) (s32 (peek32 (+ p 4))))))

(defun cursor-position ()
  "The pointer's screen position: two values."
  (let ((p (point-area)))
    (c *get-cursor-pos* p)
    (values (s32 (peek32 p)) (s32 (peek32 (+ p 4))))))

;;; ---- popup menus ------------------------------------------------------------------------------------------------

(defun choice-popup (titles x y then)
  "A popup menu of TITLES at the screen point X Y.  It is answered later: THEN is called with the index chosen, or
   NIL for none.  The menu is destroyed after its answer."
  (let ((popup (c *create-popup-menu*)))
    (loop for title in titles for i from 1
          do (c *append-menu* popup 0 i (wide1 title)))                                   ; MF_STRING, id i
    (u *set-foreground* *hwnd*)
    (ui-post *track-popup-menu* (list popup #x102 x y 0 *hwnd* 0)                       ; TPM_RETURNCMD | RIGHTBUTTON
             (lambda (r mem)
               (declare (ignore mem))
               (let ((id (logand r #xFFFFFFFF)))
                 (c *destroy-menu* popup)
                 (funcall then (and (< 0 id (1+ (length titles))) (1- id))))))
    0))

;;; ---- segmented control, toolbar, grid, collection: buttons in a panel --------------------------------------------

(defun segmented-new (a n x y w h)
  "One radio button per title, drawn as push buttons: the chosen one stays pressed."
  (let* ((titles (term-titles a n))
         (bw (/ w (max 1 (length titles))))
         (handle (choice-container :ui-segmented x y w h))
         (parent (choice-window handle))
         (buttons '()))
    (loop for title in titles for i from 0
          do (let ((b (make-window-in parent "BUTTON" title
                                      (logior #x1009 (if (zerop i) #x20000 0))             ; BS_AUTORADIO|PUSHLIKE, WS_GROUP
                                      (* i bw) 0 bw h)))
               (register-choice b :ui-segmented handle i)
               (push b buttons)))
    (setf (ctl-data (ctl-of handle)) (nreverse buttons))
    handle))

(defun toolbar-new (a n x y w h)
  (let* ((titles (term-titles a n))
         (bw (/ w (max 1 (length titles))))
         (handle (choice-container :ui-toolbar x y w h))
         (parent (choice-window handle)))
    (loop for title in titles for i from 0
          do (register-choice (make-window-in parent "BUTTON" title 0 (* i bw) 0 bw h)
                              :ui-toolbar handle i))
    handle))

(defun grid-new (a n x y w h)
  "Tiles in rows: as many columns as fit a tile of *GRID-TILE-MIN* points."
  (let* ((titles (term-titles a n))
         (cols (max 1 (floor w *grid-tile-min*)))
         (tw (/ (- w (* (1- cols) *tile-gap*)) cols))
         (handle (choice-container :ui-grid x y w h))
         (parent (choice-window handle)))
    (loop for title in titles for i from 0
          do (let ((col (mod i cols)) (row (floor i cols)))
               (register-choice (make-window-in parent "BUTTON" title 0
                                                (* col (+ tw *tile-gap*)) (* row (+ *tile-height* *tile-gap*))
                                                tw *tile-height*)
                                :ui-grid handle i)))
    handle))

(defun collection-new (a n x y w h)
  "Tiles, three to a row, under a header per section.  A row's index is among all the rows."
  (let* ((rows (let ((v (term-arg a n))) (if (listp v) v '())))
         (tw (/ (- w (* 2 *tile-gap*)) 3))
         (th *tile-height*)
         (gap *tile-gap*)
         (handle (choice-container :ui-collection x y w h))
         (parent (choice-window handle))
         (y0 0) (ry 0) (col 0) (prev :none))
    (loop for row in rows for i from 0
          do (let ((sec (row-field row :section)))
               (unless (equal sec prev)
                 (when (plusp col) (setf y0 (+ ry th gap) col 0))       ; close the row of the last section
                 (unless (eq prev :none) (incf y0 gap))
                 (make-window-in parent "STATIC" sec #x80 0 y0 w 24)     ; SS_NOPREFIX: the header
                 (incf y0 28)
                 (setf prev sec))
               (when (zerop col) (setf ry y0))
               (register-choice (make-window-in parent "BUTTON" (row-title row) 0
                                                (* col (+ tw gap)) ry tw th)
                                :ui-collection handle i)
               (incf col)
               (when (= col 3) (setf y0 (+ ry th gap) col 0))))
    handle))

(defun choice-command (e code)
  "A WM_COMMAND of the choice E's window, with the notification CODE."
  (destructuring-bind (kind h index) e
    (case kind
      (:ui-picker
       (when (= code 1)                                                    ; LBN_SELCHANGE
         (let ((sel (index-or-nil (send-to (choice-window h) #x188))))     ; LB_GETCURSEL
           (when sel (ui-event (list :select h sel))))))
      ((:ui-segmented :ui-toolbar :ui-grid :ui-collection)
       (when (zerop code) (ui-event (list :select h index))))              ; BN_CLICKED
      (:ui-menu-button
       (when (zerop code) (menu-button-click h))))))

;;; ---- menu button ----------------------------------------------------------------------------------------------

(defun menu-button-new (a n x y w h)
  (let* ((choices (term-titles a n))
         (hw (make-window-in *hwnd* "BUTTON" "options" 0 x y w h))
         (handle (new-handle (make-ctl :kind :ui-menu-button :hwnd hw :data choices))))
    (register-choice hw :ui-menu-button handle)
    handle))

(defun menu-button-click (h)
  "The menu of the menu button H, under its bottom edge: its choice is a (:menu h index) event."
  (let* ((hw (choice-window h))
         (r (rect-area)))
    (c *get-client-rect* hw r)
    (multiple-value-bind (x y) (window-to-screen hw 0 (s32 (peek32 (+ r 12))))
      (choice-popup (ctl-data (ctl-of h)) x y
                    (lambda (index) (when index (ui-event (list :menu h index))))))))

;;; ---- tab bar ----------------------------------------------------------------------------------------------------

(defun insert-tab (hw index title)
  "TCM_INSERTITEMW: a tab of TITLE at INDEX (a TCITEMW: mask TCIF_TEXT, pszText at +16)."
  (let ((item (area 2)) (text (wide1 title)))
    (zero item 40)
    (poke32 item 1)
    (poke64 (+ item 16) text)
    (send-to hw #x133E index item)))

(defun tab-bar-new (a n x y w h)
  (let* ((titles (term-titles a n))
         (hw (make-window-in *hwnd* "SysTabControl32" "" 0 x y w h))
         (handle (new-handle (make-ctl :kind :ui-tab-bar :hwnd hw))))
    (register-choice hw :ui-tab-bar handle)
    (loop for title in titles for i from 0 do (insert-tab hw i title))
    handle))

(defun tab-changed (e)
  "TCN_SELCHANGE of the tab bar E: the tab now selected (TCM_GETCURSEL) is a (:select h index) event."
  (let* ((h (second e))
         (index (index-or-nil (send-to (choice-window h) #x130B))))
    (when index (ui-event (list :select h index)))))

;;; ---- picker: a LISTBOX ---------------------------------------------------------------------------------------------

(defun picker-new (a n x y w h)
  "A listbox: LBS_NOTIFY | LBS_NOINTEGRALHEIGHT | WS_VSCROLL | WS_BORDER, a row per title."
  (let* ((hw (make-window-in *hwnd* "LISTBOX" "" #xA00101 x y w h))
         (handle (new-handle (make-ctl :kind :ui-picker :hwnd hw))))
    (register-choice hw :ui-picker handle)
    (dolist (title (term-titles a n))
      (send-to hw #x180 0 (wide1 title)))                                       ; LB_ADDSTRING
    handle))

;;; ---- list: a ListView in report mode -----------------------------------------------------------------------------

(defun list-selected (h)
  "The selected row of the list H (LVM_GETNEXTITEM, LVNI_SELECTED), or NIL."
  (index-or-nil (send-to (choice-window h) #x100C #xFFFFFFFF 2)))

(defun list-select-row (h index)
  "Select the row INDEX of the list H (LVM_SETITEMSTATE: LVIS_SELECTED | LVIS_FOCUSED)."
  (let ((it (area 2)))
    (zero it 96)
    (poke32 (+ it 12) 3) (poke32 (+ it 16) 3)
    (send-to (choice-window h) #x102B index it)))

(defun list-new (x y w h)
  "A list: a ListView in report mode, one column as wide as the list, with no header."
  (let* ((hw (make-window-in *hwnd* "SysListView32" "" #xA0400D x y w h))   ; LVS_REPORT|SINGLESEL|SHOWSELALWAYS|NOCOLUMNHEADER
         (col (area 2)))
    (zero col 64)
    (poke32 col 2)                                                            ; LVCF_WIDTH
    (poke32 (+ col 8) (max 1 (- (px w) 24)))
    (send-to hw #x1061 0 col)                                                 ; LVM_INSERTCOLUMNW
    (send-to hw #x1036 #x20 #x20)                       ; LVM_SETEXTENDEDLISTVIEWSTYLE: LVS_EX_FULLROWSELECT
    (let ((handle (new-handle (make-ctl :kind :ui-list :hwnd hw :data (make-choice-list)))))
      (register-choice hw :ui-list handle)
      handle)))

(defun list-insert-row (hw index text)
  "LVM_INSERTITEMW: a row of TEXT at INDEX (an LVITEMW: mask LVIF_TEXT, iItem, pszText at +24)."
  (let ((it (area 2)) (buf (wide1 text)))
    (zero it 96)
    (poke32 it 1)
    (poke32 (+ it 4) index)
    (poke64 (+ it 24) buf)
    (send-to hw #x104D 0 it)))

(defun list-set-data (h a n)
  "The rows of the list H: plists (:title, :subtitle), shown as title -- subtitle.  Setting them is not a selection."
  (let* ((rows (let ((v (term-arg a n))) (if (listp v) v '())))
         (st (ctl-data (ctl-of h)))
         (hw (choice-window h)))
    (setf (choice-list-rows st) rows)
    (send-to hw #x1009)                                                       ; LVM_DELETEALLITEMS
    (loop for row in rows for i from 0 do (list-insert-row hw i (row-text row)))
    (setf (choice-list-last st) (list-selected h))
    0))

(defun list-delete-row (h row)
  "Remove the row ROW from the list H: it goes from the screen, and its app hears of it: (:delete h row)."
  (let* ((st (ctl-data (ctl-of h))) (rows (choice-list-rows st)))
    (when (< -1 row (length rows))
      (setf (choice-list-rows st) (append (subseq rows 0 row) (nthcdr (1+ row) rows)))
      (send-to (choice-window h) #x1008 row)                                  ; LVM_DELETEITEM
      (setf (choice-list-last st) (list-selected h))                          ; the selection left is no choice
      (ui-event (list :delete h row)))))

(defun list-menu (h x y)
  "The row menu of the list H at the screen point X Y: its actions, then Delete (when editable), then Refresh (when
   it can be refreshed)."
  (let* ((st (ctl-data (ctl-of h)))
         (titles (append (choice-list-actions st)
                         (when (choice-list-editing st) (list "Delete"))
                         (when (choice-list-refresh st) (list "Refresh")))))
    (when titles
      (choice-popup titles x y (lambda (index) (when index (list-menu-picked h index)))))))

(defun list-menu-picked (h index)
  "The row menu's choice INDEX on the list H: an action is (:rowaction h index); Delete deletes the selected row;
   Refresh is (:refresh h).  The action's row is not sent: the app's action is for the list."
  (let* ((st (ctl-data (ctl-of h)))
         (actions (length (choice-list-actions st))))
    (cond ((< index actions) (ui-event (list :rowaction h index)))
          ((and (choice-list-editing st) (= index actions))
           (let ((sel (list-selected h))) (when sel (list-delete-row h sel))))
          (t (ui-event (list :refresh h))))))

(defun list-selection-changed (e)
  "LVN_ITEMCHANGED: the selected row, if it is a new one, is a (:select h index) event."
  (let* ((h (second e)) (st (ctl-data (ctl-of h))) (sel (list-selected h)))
    (unless (eql sel (choice-list-last st))
      (setf (choice-list-last st) sel)
      (when sel (ui-event (list :select h sel))))))

(defun list-right-click (e)
  "NM_RCLICK: the row under the pointer, if any, is selected; the row menu is shown at the pointer."
  (let* ((h (second e)) (hw (choice-window h)) (st (ctl-data (ctl-of h))))
    (multiple-value-bind (sx sy) (cursor-position)
      (multiple-value-bind (cx cy) (screen-to-window hw sx sy)
        (let ((hit (area 2)))
          (zero hit 32)
          (poke32 hit cx) (poke32 (+ hit 4) cy)
          (let ((index (index-or-nil (send-to hw #x1012 0 hit))))      ; LVM_HITTEST: the item (iItem at +12)
            (when index
              (setf (choice-list-last st) index)                       ; the selection below is no new choice
              (list-select-row h index))
            (list-menu h sx sy)))))))

(defun list-actions (h a n)
  (setf (choice-list-actions (ctl-data (ctl-of h))) (term-titles a n))
  0)

;;; ---- the messages: CHOICE-MESSAGE, run first by HANDLE-MESSAGE ----------------------------------------------------

(defun choice-message (msg hwnd wp lp)
  "The notifications of the choices and lists: true if one of them took the message."
  (declare (ignorable hwnd))
  (cond ((= msg #x111)                                                     ; WM_COMMAND: lp is the control
         (let ((e (live-choice lp)))
           (when e (choice-command e (hi16 wp)) t)))
        ((= msg #x4E)                                                      ; WM_NOTIFY: wp the control, lp the code
         (let ((e (live-choice wp)))
           (cond ((null e) nil)
                 ((and (eq (first e) :ui-tab-bar) (notify-code= lp -551))   ; TCN_SELCHANGE
                  (tab-changed e) t)
                 ((eq (first e) :ui-list)
                  (cond ((notify-code= lp -101) (list-selection-changed e) t)   ; LVN_ITEMCHANGED
                        ((notify-code= lp -5) (list-right-click e) t)           ; NM_RCLICK
                        (t nil)))
                 (t nil))))
        (t nil)))

(pushnew 'choice-message *message-hooks*)

;;; ui_set_value: a segmented control's chosen, a picker's row, a tab bar's tab.  The others take nothing (a
;;; list's refresh, a toolbar, a grid: nothing to set, so SET-VALUE does nothing for them).

(setf (gethash :ui-segmented *set-value-handlers*)
      (lambda (h v)
        (loop for b in (ctl-data (ctl-of h)) for i from 0
              do (send-to b #xF1 (if (eql i v) 1 0)))))                    ; BM_SETCHECK

(setf (gethash :ui-picker *set-value-handlers*)
      (lambda (h v) (send-to (choice-window h) #x186 (if (integerp v) v #xFFFFFFFF) 0)))   ; LB_SETCURSEL

(setf (gethash :ui-tab-bar *set-value-handlers*)
      (lambda (h v) (send-to (choice-window h) #x130C (if (integerp v) v 0) 0)))           ; TCM_SETCURSEL

;;; ---- the calls ------------------------------------------------------------------------------------------------

(defswift "ui_segmented_new" (a n x y w h) (segmented-new a n x y w h))
(defswift "ui_toolbar_new" (a n x y w h) (toolbar-new a n x y w h))
(defswift "ui_tab_bar_new" (a n x y w h) (tab-bar-new a n x y w h))
(defswift "ui_picker_new" (a n x y w h) (picker-new a n x y w h))
(defswift "ui_menu_button_new" (a n x y w h) (menu-button-new a n x y w h))
(defswift "ui_grid_new" (a n x y w h) (grid-new a n x y w h))
(defswift "ui_collection_new" (a n x y w h) (collection-new a n x y w h))
(defswift "ui_list_new" (x y w h) (list-new x y w h))
(defswift "ui_list_set_data" (h a n) (list-set-data h a n) 0)
(defswift "ui_list_actions" (h a n) (list-actions h a n))
(defswift "ui_list_editing" (h on)
  (setf (choice-list-editing (ctl-data (ctl-of h))) (not (eql on 0)))
  0)
(defswift "ui_refresh_on" (h)
  (setf (choice-list-refresh (ctl-data (ctl-of h))) t)
  0)
(defswift "ui_send_refresh" (h) (ui-event (list :refresh h)) 0)
