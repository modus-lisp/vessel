;;;; win-host.lisp -- the Windows host: what modus's host/macos and host/ios are on Apple, in Lisp, for modus.exe.
;;;;
;;;; On a Mac or a phone, vessel's platform files talk to the native host through one function, %KILN-SYS:
;;;; pseudo-syscalls 1001-1008 for the window (its size and scale, its events, the frame clock), 1010-1012 for the
;;;; speaker, and 1060 for SWIFT-CALL, which names a native function ("gpu_draw", "menu_item_new", "pad_state" ...)
;;;; and passes it up to six words.  This file answers the same calls on Windows, so ios-swift.lisp, gpu.lisp,
;;;; mac-window.lisp, sound.lisp, gamepad.lisp and notify.lisp load unchanged on top of it:
;;;;
;;;;   the window        a Win32 window of its own class, whose window procedure is native code written here
;;;;                     (%WNDPROC-CODE): it records the messages Lisp reads into a ring and lets DefWindowProcW do
;;;;                     the rest -- no Lisp runs inside a Win32 callback
;;;;   the frame clock   DwmFlush: the next composition, once a refresh
;;;;   the GPU           Direct3D 11 (gpu_*): vertex pulling from raw buffers, as Metal's shaders do; the shaders are
;;;;                     HLSL (GPU-LANGUAGE answers :HLSL), compiled at run time by d3dcompiler_47; a swap chain per
;;;;                     view, so a second window (window_new, hello3d's Map) draws too; printing (gpu_view_print)
;;;;                     reads a frame back and sends it through the print dialog.  STANDARD RANGE ONLY: HDR on
;;;;                     Windows wants a flip-model FP16 swap chain in scRGB, which this blit-model one (under child
;;;;                     windows) is not; gpu_view_hdr answers no, and the headroom is 1.0
;;;;   trackpad          a pinch is Ctrl+wheel to a Win32 window, and arrives as a :pinch gesture (type 15)
;;;;   controls          labels, buttons and a slider as Win32 child windows; a panel (glass_new) of the window's
;;;;                     own class, rounded, so its children's notifications reach the same ring
;;;;   menus             the window's menu bar; Ctrl+KEY for a Mac's Command+KEY
;;;;   the status item   an icon in the notification area: its tooltip the title, its menu on a click;
;;;;                     notifications are its balloons (toasts on Windows 10 and 11)
;;;;   alerts            MessageBoxW (OK, or OK and Cancel)
;;;;   the speaker       modus's own waveOut speaker (net/win32-audio.lisp: W32-AUDIO-OPEN ...)
;;;;   game controllers  XInput (an Xbox controller, or anything that presents as one)
;;;;   location          Windows.Devices.Geolocation's Geolocator, through modus's WinRT layer
;;;;   not on a PC       spatial sound (sound.lisp pans in its own mix), the phone's motion, the watch's link: each
;;;;                     answers that it is not here
;;;;
;;;; THE ONE THREAD.  The window is made by, and its messages are pumped on, the thread that runs the app (Win32
;;;; delivers a window's messages to its creator).  Every call here is synchronous, as the Swift calls are.
;;;;
;;;; ADDRESSES OF LISP VECTORS.  A SWIFT-CALL passes the address of a byte or float vector (text, vertices, the
;;;; frame), taken just before the call; the native host reads it before Lisp allocates again.  Here Lisp runs in
;;;; between, so a collection could move the vector first.  A Cheney collector leaves the old copy in place until
;;;; the next collection, so a READ still sees the right bytes; a WRITE into such a vector (ui_events_next,
;;;; pad_state) could be lost once, which costs an event or a frame of a controller's state.  Collections are
;;;; rare at this rate of allocation (one in thousands of frames).

(defpackage #:vessel-windows
  (:use #:cl)
  (:export #:open-window #:install-bridge #:*dpi* #:*hwnd*))

(in-package #:vessel-windows)

;;; ---- raw memory ------------------------------------------------------------------------------------------------

(defun peek8 (a) (cl-user::mem-ref a :u8))
(defun poke8 (a v) (setf (cl-user::mem-ref a :u8) (logand v 255)))
(defun peek16 (a) (+ (peek8 a) (* 256 (peek8 (+ a 1)))))
(defun poke16 (a v) (poke8 a v) (poke8 (+ a 1) (ash (logand v #xFFFF) -8)))
(defun peek32 (a) (cl-user::mem-ref a :u32))
(defun poke32 (a v) (setf (cl-user::mem-ref a :u32) (logand v #xFFFFFFFF)))
(defun peek64 (a) (modus-int::%ww-get64 a))
(defun poke64 (a v) (modus-int::%ww-put64 a v))
(defun zero (a n) (dotimes (i (ceiling n 4)) (poke32 (+ a (* 4 i)) 0)))
(defun s16 (v) (let ((x (logand v #xFFFF))) (if (>= x #x8000) (- x #x10000) x)))
(defun s32 (v) (let ((x (logand v #xFFFFFFFF))) (if (>= x #x80000000) (- x #x100000000) x)))
(defun fbits (x) (logand (modus-int::%single->bits (float x 1.0)) #xFFFFFFFF))
(defun lo16 (v) (logand v #xFFFF))
(defun hi16 (v) (logand (ash v -16) #xFFFF))

(defun utf8-text (addr n)
  "The string in the N bytes of UTF-8 at ADDR (an address, or a token for a byte vector: TOKEN)."
  (let ((arr (token-array addr)))
    (cond ((not (plusp n)) "")
          (arr (seal:utf8-decode arr :end (min n (length arr))))
          ((plusp addr) (cl-user::%utf8-get addr n))
          (t ""))))

;;; ---- vectors, without their addresses -----------------------------------------------------------------------------
;;;
;;; The Swift bridge passes a Lisp vector to the host by its ADDRESS (kiln-swift::%addr), taken just before the call:
;;; on Apple the host is native code and reads or writes it before Lisp allocates again.  Here the host is Lisp, so
;;; a collection can come between -- once a second with the generational collector -- and move the vector: a write
;;; then lands in the old copy and is lost (hello3d's view came back as handle 0).  So on Windows %ADDR hands out a
;;; TOKEN for the vector itself (INSTALL-BRIDGE), the host reads and writes the vector through it, and copies it into
;;; native memory (NATIVE-OF) only where Windows needs a pointer, at the moment it is called.  Tokens are negative,
;;; so no address is one, and they last for one call.

(defvar *tokens* (make-array 16 :fill-pointer 0 :adjustable t))

(defun token (array)
  "A token for ARRAY, for this call."
  (vector-push-extend array *tokens*)
  (- (fill-pointer *tokens*)))

(defun token-array (a)
  (and (integerp a) (minusp a) (<= (- a) (fill-pointer *tokens*)) (aref *tokens* (- -1 a))))

(defvar *stage* 0)
(defvar *stage-size* 0)

(defun native-of (a n)
  "A native address holding the N bytes A names: A itself if it is an address, else a fresh copy of the vector's
   bytes (a single-float vector's as IEEE singles) in a staging buffer, good until the next NATIVE-OF."
  (let ((arr (token-array a)))
    (if (null arr)
        a
        (progn
          (when (< *stage-size* n)
            (unless (zerop *stage*) (modus-int::w32-free *stage* *stage-size*))
            (setf *stage-size* (max n (* 2 *stage-size*) 4096) *stage* (modus-int::w32-alloc *stage-size*)))
          (if (subtypep (array-element-type arr) 'single-float)
              (dotimes (i (min (length arr) (floor n 4))) (poke32 (+ *stage* (* 4 i)) (fbits (aref arr i))))
              (dotimes (i (min (length arr) n)) (poke8 (+ *stage* i) (aref arr i))))
          *stage*))))

(defun put-floats (a vals)
  "VALS into the single-float vector A names (a token), or at the address A."
  (let ((arr (token-array a)))
    (loop for v in vals for i from 0
          do (if arr
                 (when (< i (length arr)) (setf (aref arr i) (float v 1.0)))
                 (poke32 (+ a (* 4 i)) (fbits v))))))

(defun put-bytes (a bytes)
  (let ((arr (token-array a)))
    (dotimes (i (length bytes))
      (if arr
          (when (< i (length arr)) (setf (aref arr i) (aref bytes i)))
          (poke8 (+ a i) (aref bytes i))))))

(defun ascii-at (a s)
  "S as a NUL-terminated 8-bit string at A; A."
  (dotimes (i (length s)) (poke8 (+ a i) (char-code (char s i))))
  (poke8 (+ a (length s)) 0)
  a)

(defun wide-at (a s) (modus-int::w32-put-wstr a s) a)

;;; ---- calling Windows -------------------------------------------------------------------------------------------
;;;
;;; W32-CALL maps a frame for every call; a frame of Direct3D calls is a hundred of them.  So this host keeps ONE
;;; frame (it runs on one thread) with W32-CALL's thunk in it (doubles in XMM registers, the whole of RAX back),
;;; and looks every function up once.  The block: +#x000 the argument vector, +#x100 the thunk, +#x180..#x1A0
;;; its words; then scratch: +#x1000 four struct areas of 512 bytes, +#x1800 out cells, +#x2000 and +#x3000 two
;;; UTF-16 strings, +#x4000 and +#x4400 two 8-bit strings, +#x5000 a MSG, +#x5100 a RECT, +#x5200 a POINT,
;;; +#x6000 the notification area's NOTIFYICONDATAW.

(defvar *cb* 0)

(defun area (i) (+ *cb* #x1000 (* #x200 i)))
(defun cell (i) (+ *cb* #x1800 (* 8 i)))
(defun wide1 (s) (wide-at (+ *cb* #x2000) s))
(defun wide2 (s) (wide-at (+ *cb* #x3000) s))
(defun ascii1 (s) (ascii-at (+ *cb* #x4000) s))
(defun ascii2 (s) (ascii-at (+ *cb* #x4400) s))
(defun msg-area () (+ *cb* #x5000))
(defun rect-area () (+ *cb* #x5100))
(defun point-area () (+ *cb* #x5200))
(defun nid-area () (+ *cb* #x6000))

(defun call-init ()
  (when (zerop *cb*)
    (setf *cb* (modus-int::w32-alloc #x10000 t))
    (when (zerop *cb*) (error "vessel-windows: no memory for the call frame"))
    (modus-int::%w32-thunk *cb*)))

(defun c (f &rest args)
  "Call the function at F with ARGS (integers; NIL 0, T 1); the 64 bits of RAX."
  (let ((k 0) (cb *cb*))
    (dolist (a args)
      (poke64 (+ cb (* 8 k)) (cond ((integerp a) a) ((null a) 0) ((eq a t) 1)
                                   (t (error "vessel-windows: ~S is not a word" a))))
      (incf k))
    (poke64 (+ cb #x188) f)
    (modus-int::syscall3 #x3000 (+ cb #x100) cb k)
    (peek64 (+ cb #x190))))

(defun m (obj slot &rest args)
  "COM: OBJ's method at vtable SLOT, called with OBJ and ARGS."
  (apply #'c (peek64 (+ (peek64 obj) (* 8 slot))) obj args))

(defun release (obj) (unless (zerop obj) (m obj 2)) 0)

(defun failed-p (hr) (>= (logand hr #xFFFFFFFF) #x80000000))

(defun check (hr what)
  (when (failed-p hr)
    (error "vessel-windows: ~A failed: #x~8,'0X ~A" what (logand hr #xFFFFFFFF)
           (or (ignore-errors (modus-int::w32-error-text hr)) "")))
  hr)

(defun gc-safe (f &rest args)
  "Call F with ARGS as a blocking call: a collection on another thread need not wait for it."
  (let ((k 0))
    (dolist (a args) (poke64 (+ *cb* (* 8 k)) a) (incf k))
    (modus-int::%gc-safe-block-6 #x3000 f *cb* k 0)))

(defmacro defprocs (&rest specs)
  "Each (VAR DLL NAME [:optional]): VAR holds the function's address once PROCS-INIT has looked it up."
  `(progn
     ,@(loop for s in specs collect `(defvar ,(first s) 0))
     (defparameter *procs* ',specs)))

(defprocs
  (*register-class* "user32.dll" "RegisterClassExW")
  (*create-window* "user32.dll" "CreateWindowExW")
  (*def-window-proc* "user32.dll" "DefWindowProcW")
  (*show-window* "user32.dll" "ShowWindow")
  (*update-window* "user32.dll" "UpdateWindow")
  (*set-foreground* "user32.dll" "SetForegroundWindow")
  (*peek-message* "user32.dll" "PeekMessageW")
  (*translate-message* "user32.dll" "TranslateMessage")
  (*dispatch-message* "user32.dll" "DispatchMessageW")
  (*get-client-rect* "user32.dll" "GetClientRect")
  (*adjust-rect* "user32.dll" "AdjustWindowRectEx")
  (*load-cursor* "user32.dll" "LoadCursorW")
  (*load-icon* "user32.dll" "LoadIconW")
  (*set-cursor* "user32.dll" "SetCursor")
  (*set-cursor-pos* "user32.dll" "SetCursorPos")
  (*get-cursor-pos* "user32.dll" "GetCursorPos")
  (*clip-cursor* "user32.dll" "ClipCursor")
  (*map-points* "user32.dll" "MapWindowPoints")
  (*validate-rect* "user32.dll" "ValidateRect")
  (*set-window-text* "user32.dll" "SetWindowTextW")
  (*set-focus* "user32.dll" "SetFocus")
  (*set-parent* "user32.dll" "SetParent")
  (*destroy-window* "user32.dll" "DestroyWindow")
  (*send-message* "user32.dll" "SendMessageW")
  (*create-menu* "user32.dll" "CreateMenu")
  (*create-popup-menu* "user32.dll" "CreatePopupMenu")
  (*append-menu* "user32.dll" "AppendMenuW")
  (*set-menu* "user32.dll" "SetMenu")
  (*draw-menu-bar* "user32.dll" "DrawMenuBar")
  (*check-menu-item* "user32.dll" "CheckMenuItem")
  (*enable-menu-item* "user32.dll" "EnableMenuItem")
  (*track-popup-menu* "user32.dll" "TrackPopupMenu")
  (*message-box* "user32.dll" "MessageBoxW")
  (*set-window-rgn* "user32.dll" "SetWindowRgn")
  (*set-window-pos* "user32.dll" "SetWindowPos")
  (*is-iconic* "user32.dll" "IsIconic")
  (*get-dpi-for-window* "user32.dll" "GetDpiForWindow" :optional)
  (*get-dpi-for-system* "user32.dll" "GetDpiForSystem" :optional)
  (*set-dpi-context* "user32.dll" "SetProcessDpiAwarenessContext" :optional)
  (*set-dpi-aware* "user32.dll" "SetProcessDPIAware")
  (*set-text-color* "gdi32.dll" "SetTextColor")
  (*set-bk-color* "gdi32.dll" "SetBkColor")
  (*create-brush* "gdi32.dll" "CreateSolidBrush")
  (*create-font* "gdi32.dll" "CreateFontW")
  (*create-round-rgn* "gdi32.dll" "CreateRoundRectRgn")
  (*get-module-handle* "kernel32.dll" "GetModuleHandleW")
  (*sleep* "kernel32.dll" "Sleep")
  (*drag-accept* "shell32.dll" "DragAcceptFiles")
  (*drag-query* "shell32.dll" "DragQueryFileW")
  (*drag-finish* "shell32.dll" "DragFinish")
  (*notify-icon* "shell32.dll" "Shell_NotifyIconW")
  (*init-common-controls* "comctl32.dll" "InitCommonControlsEx")
  (*dwm-flush* "dwmapi.dll" "DwmFlush")
  (*memcpy* "ntdll.dll" "memcpy")
  (*d3d11-create-device* "d3d11.dll" "D3D11CreateDevice")
  (*create-dc* "gdi32.dll" "CreateDCW")
  (*delete-dc* "gdi32.dll" "DeleteDC")
  (*start-doc* "gdi32.dll" "StartDocW")
  (*end-doc* "gdi32.dll" "EndDoc")
  (*start-page* "gdi32.dll" "StartPage")
  (*end-page* "gdi32.dll" "EndPage")
  (*get-device-caps* "gdi32.dll" "GetDeviceCaps")
  (*stretch-dibits* "gdi32.dll" "StretchDIBits")
  (*set-stretch-mode* "gdi32.dll" "SetStretchBltMode")
  (*set-brush-org* "gdi32.dll" "SetBrushOrgEx")
  (*print-dlg* "comdlg32.dll" "PrintDlgW")
  (*global-free* "kernel32.dll" "GlobalFree")
  (*init-cs* "kernel32.dll" "InitializeCriticalSection")
  (*enter-cs* "kernel32.dll" "EnterCriticalSection")
  (*leave-cs* "kernel32.dll" "LeaveCriticalSection")
  (*create-thread* "kernel32.dll" "CreateThread")
  (*switch-thread* "kernel32.dll" "SwitchToThread")
  (*wait-object* "kernel32.dll" "WaitForSingleObjectEx")
  (*get-raw-input* "user32.dll" "GetRawInputData")
  (*register-raw-input* "user32.dll" "RegisterRawInputDevices")
  (*co-initialize* "ole32.dll" "CoInitializeEx")
  (*co-create* "ole32.dll" "CoCreateInstance")
  (*create-event* "kernel32.dll" "CreateEventW")
  (*set-thread-priority* "kernel32.dll" "SetThreadPriority")
  (*wait-single* "kernel32.dll" "WaitForSingleObject")
  (*create-file* "kernel32.dll" "CreateFileW")
  (*write-file* "kernel32.dll" "WriteFile")
  (*close-handle* "kernel32.dll" "CloseHandle")
  (*set-process-info* "kernel32.dll" "SetProcessInformation" :optional)
  (*current-process* "kernel32.dll" "GetCurrentProcess")
  (*get-message* "user32.dll" "GetMessageW")
  (*post-message* "user32.dll" "PostMessageW")
  (*d3d-compile* "d3dcompiler_47.dll" "D3DCompile")
  (*xinput-get* "xinput1_4.dll" "XInputGetState" :optional)
  (*xinput-set* "xinput1_4.dll" "XInputSetState" :optional))

(defun procs-init ()
  (dolist (p *procs*)
    (destructuring-bind (var dll name &optional optional) p
      (multiple-value-bind (f why) (modus-int::w32-proc dll name)
        (when (and (zerop f) (not optional)) (error "vessel-windows: ~A" why))
        (setf (symbol-value var) f)))))

;;; ---- handles ---------------------------------------------------------------------------------------------------------
;;;
;;; A handle is a number this host gives out: for a control, a menu, an item, an alert, a GPU object.

(defstruct ctl kind (hwnd 0) (value nil) (data nil))

(defvar *ctls* (make-hash-table))
(defvar *by-hwnd* (make-hash-table))
(defvar *next-handle* 1)
(defvar *menu-ids* (make-hash-table) "A menu command's id -> its item's handle.")

;;; ---- the window procedure ----------------------------------------------------------------------------------------
;;;
;;; Native code in a page of its own, made once.  Win32 calls it inside DispatchMessageW (and inside the modal loops
;;; of a menu, a message box, a resize), on this thread.  It appends (msg, hwnd, wParam, lParam) to the ring when the
;;; message is one Lisp reads (a bitmap of the first 1024 message numbers, and every WM_APP message), then answers:
;;;   for the main window: WM_ERASEBKGND 1 (Direct3D paints it), WM_PAINT validated (a frame comes anyway),
;;;   WM_CLOSE 0 (Lisp decides), WM_SETCURSOR over the client area the pointer Lisp chose (+#x40; 0 hides it);
;;;   for any window: WM_CTLCOLORSTATIC / WM_CTLCOLORBTN the panel's colours (text +#x48, background +#x50, brush
;;;   +#x58), so labels and the slider sit on the panel;
;;;   everything else: DefWindowProcW.
;;; The page: +#x00 the ring's count (dword)  +#x08 its capacity  +#x10 the main window  +#x18 DefWindowProcW
;;; +#x20 SetTextColor  +#x28 SetBkColor  +#x30 SetCursor  +#x38 ValidateRect  +#x40..#x58 as above
;;; +#x60 the bitmap (128 bytes)  +#xE0 EnterCriticalSection  +#xE8 LeaveCriticalSection  +#xF0 GetRawInputData
;;; +#x100 the ring's lock (a CRITICAL_SECTION: the window procedure runs on the UI thread, TAKE-RING on the app's)
;;; +#x150 what to record next (msg hwnd wParam lParam)  +#x178 a size  +#x180 a RAWINPUT  +#x200 the code
;;; +#x1000 the ring, 32 bytes an entry.  WM_INPUT is read where it arrives (GetRawInputData: DefWindowProcW frees
;;; it) and recorded as (WM_INPUT hwnd dx dy), the mouse's own motion.

(defconstant +ring-capacity+ 4096)

(defun assemble (items)
  "Bytes from ITEMS: bytes, (:label K), (:j (OPCODE-BYTES) K) with a 32-bit displacement to K, (:u64 V)."
  (let ((labels (make-hash-table)) (pc 0))
    (dolist (it items)
      (cond ((integerp it) (incf pc))
            ((eq (first it) :label) (setf (gethash (second it) labels) pc))
            ((eq (first it) :j) (incf pc (+ 4 (length (second it)))))
            ((eq (first it) :u64) (incf pc 8))))
    (let ((out '()) (pc 0))
      (dolist (it items (values (nreverse out) labels))
        (cond ((integerp it) (push it out) (incf pc))
              ((eq (first it) :label))
              ((eq (first it) :j)
               (dolist (b (second it)) (push b out))
               (incf pc (+ 4 (length (second it))))
               (let ((rel (- (gethash (third it) labels) pc)))
                 (dotimes (i 4) (push (ldb (byte 8 (* 8 i)) rel) out))))
              ((eq (first it) :u64)
               (dotimes (i 8) (push (ldb (byte 8 (* 8 i)) (second it)) out))
               (incf pc 8)))))))

(defun wndproc-code (page)
  (let ((jae '(#x0F #x83)) (jne '(#x0F #x85)) (je '(#x0F #x84)) (jmp '(#xE9)) (call '(#xE8)))
    (assemble
     `(#x48 #x89 #x4C #x24 #x08              ; mov [rsp+8],rcx    hwnd   (the caller's home slots)
       #x48 #x89 #x54 #x24 #x10              ; mov [rsp+16],rdx   msg
       #x4C #x89 #x44 #x24 #x18              ; mov [rsp+24],r8    wParam
       #x4C #x89 #x4C #x24 #x20              ; mov [rsp+32],r9    lParam
       #x53                                  ; push rbx
       #x48 #x83 #xEC #x30                   ; sub rsp,0x30   (aligned; home slots and a fifth argument)
       ;; from here the arguments are at [rsp+0x40] hwnd, [rsp+0x48] msg, [rsp+0x50] wParam, [rsp+0x58] lParam
       #x48 #xBB (:u64 ,page)                ; mov rbx,PAGE
       #x81 #xFA #xFF #x00 #x00 #x00 (:j ,je :input)     ; WM_INPUT: the mouse's own motion
       #x81 #xFA #x00 #x80 #x00 #x00 (:j ,jae :rec)      ; WM_APP and up: record
       #x81 #xFA #x00 #x04 #x00 #x00 (:j ,jae :after)    ; past the bitmap: not
       #x0F #xA3 #x53 #x60 (:j ,jae :after)              ; bt [rbx+0x60],edx; clear: not
       (:label :rec)
       #x89 #xD0                             ; mov eax,edx
       #x48 #x89 #x83 #x50 #x01 #x00 #x00    ; mov [rbx+0x150],rax   what to record
       #x48 #x89 #x8B #x58 #x01 #x00 #x00    ; mov [rbx+0x158],rcx
       #x4C #x89 #x83 #x60 #x01 #x00 #x00    ; mov [rbx+0x160],r8
       #x4C #x89 #x8B #x68 #x01 #x00 #x00    ; mov [rbx+0x168],r9
       (:j ,call :record)
       #x48 #x8B #x4C #x24 #x40              ; the arguments back
       #x8B #x54 #x24 #x48
       #x4C #x8B #x44 #x24 #x50
       #x4C #x8B #x4C #x24 #x58
       (:j ,jmp :after)
       (:label :input)
       ;; GetRawInputData(lParam, RID_INPUT, page+0x180, &size, sizeof RAWINPUTHEADER): read now, before
       ;; DefWindowProcW frees it; a mouse's relative motion is recorded as (WM_INPUT hwnd dx dy)
       #xC7 #x83 #x78 #x01 #x00 #x00 #x40 #x00 #x00 #x00   ; mov dword [rbx+0x178],64
       #x4C #x89 #xC9                        ; mov rcx,r9
       #xBA #x03 #x00 #x00 #x10              ; mov edx,RID_INPUT
       #x4C #x8D #x83 #x80 #x01 #x00 #x00    ; lea r8,[rbx+0x180]
       #x4C #x8D #x8B #x78 #x01 #x00 #x00    ; lea r9,[rbx+0x178]
       #x48 #xC7 #x44 #x24 #x20 #x18 #x00 #x00 #x00     ; mov qword [rsp+0x20],24
       #xFF #x93 #xF0 #x00 #x00 #x00         ; call GetRawInputData
       #x83 #xBB #x80 #x01 #x00 #x00 #x00 (:j ,jne :def)   ; dwType: not the mouse
       #xF6 #x83 #x98 #x01 #x00 #x00 #x01 (:j ,jne :def)   ; usFlags MOUSE_MOVE_ABSOLUTE: not motion
       #x48 #xC7 #x83 #x50 #x01 #x00 #x00 #xFF #x00 #x00 #x00   ; mov qword [rbx+0x150],WM_INPUT
       #x48 #x8B #x44 #x24 #x40              ; mov rax,[rsp+0x40]   hwnd
       #x48 #x89 #x83 #x58 #x01 #x00 #x00    ; mov [rbx+0x158],rax
       #x48 #x63 #x83 #xA4 #x01 #x00 #x00    ; movsxd rax,dword [rbx+0x1A4]   lLastX
       #x48 #x89 #x83 #x60 #x01 #x00 #x00    ; mov [rbx+0x160],rax
       #x48 #x63 #x83 #xA8 #x01 #x00 #x00    ; movsxd rax,dword [rbx+0x1A8]   lLastY
       #x48 #x89 #x83 #x68 #x01 #x00 #x00    ; mov [rbx+0x168],rax
       (:j ,call :record)
       (:j ,jmp :def)
       (:label :after)
       #x48 #x3B #x4B #x10 (:j ,jne :ctl?)   ; cmp rcx,[rbx+0x10]: not the main window
       #x83 #xFA #x14 (:j ,je :one)          ; WM_ERASEBKGND
       #x83 #xFA #x0F (:j ,je :paint)        ; WM_PAINT
       #x83 #xFA #x10 (:j ,je :zero)         ; WM_CLOSE
       #x83 #xFA #x20 (:j ,jne :ctl?)        ; WM_SETCURSOR
       #x66 #x41 #x83 #xF9 #x01 (:j ,jne :def)   ; cmp r9w,HTCLIENT
       #x48 #x8B #x4B #x40                   ; mov rcx,[rbx+0x40]
       #xFF #x53 #x30 (:j ,jmp :one)         ; call SetCursor
       (:label :ctl?)
       #x81 #xFA #x38 #x01 #x00 #x00 (:j ,je :ctl)   ; WM_CTLCOLORSTATIC
       #x81 #xFA #x35 #x01 #x00 #x00 (:j ,je :ctl)   ; WM_CTLCOLORBTN
       (:j ,jmp :def)
       (:label :ctl)
       #x4C #x89 #xC1                        ; mov rcx,r8        the DC
       #x8B #x53 #x48                        ; mov edx,[rbx+0x48]
       #xFF #x53 #x20                        ; call SetTextColor
       #x48 #x8B #x4C #x24 #x50              ; mov rcx,[rsp+0x50]
       #x8B #x53 #x50                        ; mov edx,[rbx+0x50]
       #xFF #x53 #x28                        ; call SetBkColor
       #x48 #x8B #x43 #x58 (:j ,jmp :ret)    ; mov rax,[rbx+0x58]  the brush
       (:label :paint)
       #x31 #xD2 #xFF #x53 #x38              ; ValidateRect(hwnd, NULL)
       (:label :zero)
       #x31 #xC0 (:j ,jmp :ret)
       (:label :one)
       #xB8 #x01 #x00 #x00 #x00 (:j ,jmp :ret)
       (:label :def)
       #x48 #x8B #x4C #x24 #x40              ; the four arguments back
       #x48 #x8B #x54 #x24 #x48
       #x4C #x8B #x44 #x24 #x50
       #x4C #x8B #x4C #x24 #x58
       #xFF #x53 #x18                        ; call DefWindowProcW
       (:label :ret)
       #x48 #x83 #xC4 #x30 #x5B #xC3         ; add rsp,0x30; pop rbx; ret
       (:label :record)                      ; append [rbx+0x150..0x168] to the ring, under its lock
       #x48 #x83 #xEC #x28                   ; sub rsp,0x28
       #x48 #x8D #x8B #x00 #x01 #x00 #x00    ; lea rcx,[rbx+0x100]
       #xFF #x93 #xE0 #x00 #x00 #x00         ; call EnterCriticalSection
       #x8B #x03                             ; mov eax,[rbx]      count
       #x3B #x43 #x08 (:j ,jae :rec-unlock)  ; cmp eax,[rbx+8]    full: dropped
       #x48 #xC1 #xE0 #x05                   ; shl rax,5
       #x4C #x8D #x94 #x03 #x00 #x10 #x00 #x00   ; lea r10,[rbx+rax+0x1000]
       #x48 #x8B #x83 #x50 #x01 #x00 #x00 #x49 #x89 #x02         ; [r10]    <- msg
       #x48 #x8B #x83 #x58 #x01 #x00 #x00 #x49 #x89 #x42 #x08    ; [r10+8]  <- hwnd
       #x48 #x8B #x83 #x60 #x01 #x00 #x00 #x49 #x89 #x42 #x10    ; [r10+16] <- wParam
       #x48 #x8B #x83 #x68 #x01 #x00 #x00 #x49 #x89 #x42 #x18    ; [r10+24] <- lParam
       #xFF #x03                             ; inc dword [rbx]
       (:label :rec-unlock)
       #x48 #x8D #x8B #x00 #x01 #x00 #x00    ; lea rcx,[rbx+0x100]
       #xFF #x93 #xE8 #x00 #x00 #x00         ; call LeaveCriticalSection
       #x48 #x83 #xC4 #x28 #xC3))))          ; add rsp,0x28; ret

(defparameter *recorded*
  (append '(#x05 #x07 #x08 #x10 #x1C #x111 #x114 #x233)
          (loop for m from #x100 to #x10F collect m)
          (loop for m from #x200 to #x20E collect m))
  "The messages the ring keeps: size, focus, close, the app's activation, a command (a menu, a button), a
   slider, files dropped, keys, the mouse -- and WM_APP and up (the notification area's).")

(defvar *page* 0)
(defvar *pages* '() "Every window procedure page: the main window's, then one a window_new.")

(defun make-page ()
  (let ((page (modus-int::w32-alloc (+ #x1000 (* 32 +ring-capacity+)) t)))
    (when (zerop page) (error "vessel-windows: no memory for the window procedure"))
    (let ((code (wndproc-code page)))
      (loop for b in code for i from 0 do (poke8 (+ page #x200 i) b)))
    (poke64 (+ page #xE0) *enter-cs*)
    (poke64 (+ page #xE8) *leave-cs*)
    (poke64 (+ page #xF0) *get-raw-input*)
    (c *init-cs* (+ page #x100))
    (poke32 (+ page 8) +ring-capacity+)
    (poke64 (+ page #x18) *def-window-proc*)
    (poke64 (+ page #x20) *set-text-color*)
    (poke64 (+ page #x28) *set-bk-color*)
    (poke64 (+ page #x30) *set-cursor*)
    (poke64 (+ page #x38) *validate-rect*)
    (dolist (msg *recorded*)
      (let ((a (+ page #x60 (floor msg 8))))
        (poke8 a (logior (peek8 a) (ash 1 (mod msg 8))))))
    page))

(defvar *ring-copy* 0 "Where TAKE-RING copies a ring to, under its lock, before reading it.")

(defun take-ring (page)
  "PAGE's recorded messages, (msg hwnd wparam lparam) oldest first; its ring emptied.  Copied out under its lock (the
   window procedure appends on the UI thread), then read."
  (when (zerop *ring-copy*) (setf *ring-copy* (modus-int::w32-alloc (* 32 +ring-capacity+))))
  (c *enter-cs* (+ page #x100))
  (let ((n (peek32 page)))
    (unless (zerop n) (c *memcpy* *ring-copy* (+ page #x1000) (* 32 n)))
    (poke32 page 0)
    (c *leave-cs* (+ page #x100))
    (let ((out '()))
      (dotimes (i n)
        (let ((e (+ *ring-copy* (* 32 i))))
          (push (list (peek32 e) (peek64 (+ e 8)) (peek64 (+ e 16)) (peek64 (+ e 24))) out)))
      (nreverse out))))

;;; ---- the UI thread ----------------------------------------------------------------------------------------------------
;;;
;;; THE WINDOWS LIVE ON A THREAD OF THEIR OWN, as an AppKit app's live on its main thread while the modus image runs on
;;; another (modus's host/macos).  Win32 runs a title-bar drag, a resize, an open menu, a message box as a MODAL LOOP
;;; inside DispatchMessage, on the windows' thread, until the person lets go: when that thread was the app's, every
;;; frame and the speaker stopped for as long.  Now that thread is native code, written here, that only pumps messages
;;; (GetMessageW / TranslateMessage / DispatchMessageW); the app's thread reads what the window procedures recorded
;;; (TAKE-RING) and draws, and a modal loop holds up nothing of it.
;;;
;;; WHAT MUST HAPPEN ON THE WINDOWS' THREAD -- making a window or a control, focus, the pointer's shape, a menu bar, a
;;; message box, the notification area's menu -- is asked of it: U posts a request to a message-only "call" window of
;;; that thread, whose window procedure (native) makes the call and marks it done, and waits; UI-POST does not wait,
;;; and the answer comes back as WM_APP+21 (wParam the request's id, lParam its result) to the main window's ring.  A
;;; posted message and not a thread message, because a modal loop dispatches the one and drops the other: a request
;;; made during a drag is still answered.
;;;
;;; The block: +#x00 GetMessageW  +#x08 TranslateMessage  +#x10 DispatchMessageW  +#x18 PostMessageW  +#x20
;;; DefWindowProcW  +#x28 the call window (set by the thread)  +#x30 the request that makes it  +#x40 a MSG
;;; +#x100 the code  +#x400 that request  +#x600 the call window's class name.
;;; A request: +0 the function  +16 twelve arguments  +112 the result  +120 done  +128 the window to tell (0: none)
;;; +136 the id it is told with.

(defvar *ui* 0)
(defvar *call-hwnd* 0)
(defvar *ui-req* 0 "The one request U uses (the app's thread makes one at a time).")

(defun ui-code (block)
  (let ((jz '(#x0F #x84)) (jne '(#x0F #x85)) (jle '(#x0F #x8E)) (jmp '(#xE9)) (call '(#xE8)))
    (assemble
     `((:label :do-request)                 ; rsi a request, rbx the block; called with the stack aligned
       #x48 #x83 #xEC #x68                  ; sub rsp,0x68
       ,@(loop for i from 4 below 12
               append (list #x48 #x8B #x46 (+ 16 (* 8 i))                    ; mov rax,[rsi+16+8i]
                            #x48 #x89 #x44 #x24 (+ #x20 (* 8 (- i 4)))))     ; mov [rsp+0x20+..],rax
       #x48 #x8B #x4E #x10                  ; mov rcx,[rsi+16]
       #x48 #x8B #x56 #x18                  ; mov rdx,[rsi+24]
       #x4C #x8B #x46 #x20                  ; mov r8,[rsi+32]
       #x4C #x8B #x4E #x28                  ; mov r9,[rsi+40]
       #xFF #x16                            ; call [rsi]
       #x48 #x89 #x46 #x70                  ; mov [rsi+112],rax
       #x48 #xC7 #x46 #x78 #x01 0 0 0       ; mov qword [rsi+120],1     done
       #x48 #x8B #x8E #x80 #x00 #x00 #x00   ; mov rcx,[rsi+128]         a window to tell?
       #x48 #x85 #xC9 (:j ,jz :req-ret)
       #xBA #x15 #x80 #x00 #x00             ; mov edx,WM_APP+21
       #x4C #x8B #x86 #x88 #x00 #x00 #x00   ; mov r8,[rsi+136]          its id
       #x4C #x8B #x4E #x70                  ; mov r9,[rsi+112]          its result
       #xFF #x53 #x18                       ; call PostMessageW
       (:label :req-ret)
       #x48 #x83 #xC4 #x68 #xC3             ; add rsp,0x68; ret
       (:label :call-wndproc)               ; the call window's procedure
       #x81 #xFA #x14 #x80 #x00 #x00 (:j ,jne :call-def)   ; WM_APP+20: a request, at wParam
       #x53 #x56 #x48 #x83 #xEC #x08        ; push rbx; push rsi; sub rsp,8
       #x48 #xBB (:u64 ,block)              ; mov rbx,BLOCK
       #x4C #x89 #xC6                       ; mov rsi,r8
       (:j ,call :do-request)
       #x48 #x83 #xC4 #x08 #x5E #x5B        ; add rsp,8; pop rsi; pop rbx
       #x31 #xC0 #xC3                       ; xor eax,eax; ret
       (:label :call-def)
       #x48 #xB8 (:u64 ,block)              ; mov rax,BLOCK
       #x48 #x8B #x40 #x20                  ; mov rax,[rax+0x20]
       #xFF #xE0                            ; jmp DefWindowProcW
       (:label :thread)                     ; the thread: rcx the block
       #x53 #x56 #x48 #x83 #xEC #x28        ; push rbx; push rsi; sub rsp,0x28
       #x48 #x89 #xCB                       ; mov rbx,rcx
       #x48 #x8B #x73 #x30                  ; mov rsi,[rbx+0x30]   make the call window
       (:j ,call :do-request)
       #x48 #x8B #x46 #x70 #x48 #x89 #x43 #x28   ; mov rax,[rsi+112]; mov [rbx+0x28],rax
       (:label :pump)
       #x48 #x8D #x4B #x40 #x31 #xD2 #x45 #x31 #xC0 #x45 #x31 #xC9   ; rcx=&msg, rdx=r8=r9=0
       #xFF #x13                            ; call GetMessageW
       #x85 #xC0 (:j ,jle :out)             ; 0 WM_QUIT, -1 an error
       #x48 #x8D #x4B #x40 #xFF #x53 #x08   ; TranslateMessage(&msg)
       #x48 #x8D #x4B #x40 #xFF #x53 #x10   ; DispatchMessageW(&msg)
       (:j ,jmp :pump)
       (:label :out)
       #x48 #x83 #xC4 #x28 #x5E #x5B #x31 #xC0 #xC3))))

(defun fill-request (r f args &key (notify 0) (id 0))
  (when (> (length args) 12) (error "vessel-windows: a UI call takes at most 12 arguments"))
  (poke64 r f)
  (loop for a in args for i from 0
        do (poke64 (+ r 16 (* 8 i)) (cond ((integerp a) a) ((null a) 0) ((eq a t) 1) (t 0))))
  (poke64 (+ r 112) 0) (poke64 (+ r 120) 0) (poke64 (+ r 128) notify) (poke64 (+ r 136) id)
  r)

(defun start-ui-thread ()
  (let ((block (modus-int::w32-alloc #x1000 t)))
    (poke64 block *get-message*) (poke64 (+ block 8) *translate-message*) (poke64 (+ block #x10) *dispatch-message*)
    (poke64 (+ block #x18) *post-message*) (poke64 (+ block #x20) *def-window-proc*)
    (multiple-value-bind (code labels) (ui-code block)
      (loop for b in code for i from 0 do (poke8 (+ block #x100 i) b))
      ;; the call window's class, then the request that makes it (message-only: HWND_MESSAGE, -3)
      (let ((wc (area 0)) (class (wide-at (+ block #x600) "VesselCall")))
        (zero wc 80)
        (poke32 wc 80) (poke64 (+ wc 8) (+ block #x100 (gethash :call-wndproc labels)))
        (poke64 (+ wc 24) *hinst*) (poke64 (+ wc 64) class)
        (c *register-class* wc)
        (fill-request (+ block #x400) *create-window* (list 0 class 0 0 0 0 0 0 -3 0 *hinst* 0))
        (poke64 (+ block #x30) (+ block #x400)))
      (setf *ui* block *ui-req* (modus-int::w32-alloc 256))
      (c *create-thread* 0 0 (+ block #x100 (gethash :thread labels)) block 0 0)
      (loop repeat 100000 until (/= 0 (peek64 (+ block #x28))) do (c *switch-thread*))
      (setf *call-hwnd* (peek64 (+ block #x28)))
      (when (zerop *call-hwnd*) (error "vessel-windows: the UI thread did not start")))))

(defun u (f &rest args)
  "F called with ARGS on the windows' thread; its result.  Waits."
  (let ((r (fill-request *ui-req* f args)))
    (c *post-message* *call-hwnd* #x8014 r 0)
    (loop until (/= 0 (peek64 (+ r 120))) do (c *switch-thread*))
    (peek64 (+ r 112))))

(defvar *posted* (make-hash-table) "An asynchronous request's id -> (function of its result . its memory).")
(defvar *next-post* (ash 1 40))

(defun ui-post (f args then &optional (extra 0))
  "F called with ARGS on the windows' thread, without waiting; THEN gets its result and the request's own memory
   later (WM_APP+21).  An ARG may be a function of that memory (EXTRA bytes from +#x100 of it): what the call reads
   after this returns, such as a message box's strings, must live there."
  (let* ((id (incf *next-post*))
         (size (+ #x100 extra))
         (r (modus-int::w32-alloc size))
         (args (mapcar (lambda (a) (if (functionp a) (funcall a (+ r #x100)) a)) args)))
    (setf (gethash id *posted*) (list then r size))
    (fill-request r f args :notify *hwnd* :id id)
    (c *post-message* *call-hwnd* #x8014 r 0)
    id))

(defun posted-answer (id result)
  (let ((p (gethash id *posted*)))
    (when p
      (remhash id *posted*)
      (destructuring-bind (then r size) p
        (unwind-protect (funcall then result (+ r #x100))
          (modus-int::w32-free r size))))))

;;; ---- the window --------------------------------------------------------------------------------------------------

(defvar *hwnd* 0)
(defvar *hinst* 0)
(defvar *dpi* 96)
(defvar *w* 0 "The client area's size, pixels.")
(defvar *h* 0)
(defvar *font* 0)
(defvar *arrow* 0)
(defparameter *panel-rgb* #x1f2a38 "The panels' colour, #xRRGGBB.")
(defparameter *text-rgb* #xf2f2ee)

(defun colorref (rgb) (logior (ash (ldb (byte 8 0) rgb) 16) (ash (ldb (byte 8 8) rgb) 8) (ldb (byte 8 16) rgb)))
(defun px (points) (round (* points *dpi*) 96))
(defun ui-scale () (max 1 (round *dpi* 96)))

(defun register-class (name cursor brush &optional (page *page*))
  (let ((wc (area 0)))
    (zero wc 80)
    (poke32 wc 80)
    (poke64 (+ wc 8) (+ page #x200))
    (poke64 (+ wc 24) *hinst*)
    (let ((icon (c *load-icon* 0 32512)))                  ; IDI_APPLICATION
      (poke64 (+ wc 32) icon)
      (poke64 (+ wc 72) icon))
    (poke64 (+ wc 40) cursor)
    (poke64 (+ wc 48) brush)
    (poke64 (+ wc 64) (wide2 name))
    (c *register-class* wc)))

(defconstant +main-style+ #x02CF0000 "WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN")

(defun client-size ()
  (let ((r (rect-area)))
    (c *get-client-rect* *hwnd* r)
    (setf *w* (s32 (peek32 (+ r 8))) *h* (s32 (peek32 (+ r 12))))))

(defun open-window (title width height)
  "Open the app's window, WIDTH x HEIGHT points, titled TITLE.  The rest of this host acts on it."
  (call-init)
  (procs-init)
  (if (plusp *set-dpi-context*)
      (c *set-dpi-context* -4)                             ; per-monitor aware, v2
      (c *set-dpi-aware*))
  ;; NOT POWER-THROTTLED: Windows 11 slows a process's threads and timers (EcoQoS) while none of its windows is in
  ;; front; a game drawing on the refresh and feeding a speaker stalls in the background.  PROCESS_POWER_THROTTLING_STATE
  ;; {Version 1, ControlMask EXECUTION_SPEED | IGNORE_TIMER_RESOLUTION, StateMask 0}: both off.
  (when (plusp *set-process-info*)
    (let ((st (area 3)))
      (poke32 st 1) (poke32 (+ st 4) 5) (poke32 (+ st 8) 0)
      (c *set-process-info* (c *current-process*) 4 st 12)))
  (setf *hinst* (c *get-module-handle* 0)
        *page* (make-page)
        *pages* (list *page*)
        *arrow* (c *load-cursor* 0 32512))
  (let ((brush (c *create-brush* (colorref *panel-rgb*))))
    (poke64 (+ *page* #x40) *arrow*)
    (poke32 (+ *page* #x48) (colorref *text-rgb*))
    (poke32 (+ *page* #x50) (colorref *panel-rgb*))
    (poke64 (+ *page* #x58) brush)
    (register-class "VesselWindow" 0 0)
    (register-class "VesselPanel" *arrow* brush))
  (setf *dpi* (if (plusp *get-dpi-for-system*) (logand (c *get-dpi-for-system*) #xFFFF) 96))
  (let ((r (rect-area)))
    (zero r 16)
    (poke32 (+ r 8) (px width)) (poke32 (+ r 12) (px height))
    (c *adjust-rect* r +main-style+ 1 0)
    (let ((ow (- (s32 (peek32 (+ r 8))) (s32 (peek32 r))))
          (oh (- (s32 (peek32 (+ r 12))) (s32 (peek32 (+ r 4))))))
      (start-ui-thread)
      (setf *hwnd* (u *create-window* 0 (wide1 "VesselWindow") (wide2 title) +main-style+
                      #x80000000 #x80000000 ow oh 0 0 *hinst* 0))))
  (when (zerop *hwnd*) (error "vessel-windows: CreateWindowExW failed"))
  (poke64 (+ *page* #x10) *hwnd*)
  (when (plusp *get-dpi-for-window*) (setf *dpi* (logand (c *get-dpi-for-window* *hwnd*) #xFFFF)))
  (setf *font* (c *create-font* (- (px 14)) 0 0 0 400 0 0 0 1 0 0 5 0 (wide1 "Segoe UI")))
  (let ((icc (area 1)))                                    ; the slider's class
    (poke32 icc 8) (poke32 (+ icc 4) 4)
    (c *init-common-controls* icc))
  (u *show-window* *hwnd* 5)
  (u *update-window* *hwnd*)
  (u *set-foreground* *hwnd*)
  (u *drag-accept* *hwnd* 1)
  (let ((rid (area 3)))                                    ; RAWINPUTDEVICE: the mouse (page 1, usage 2), to this window
    (poke16 rid 1) (poke16 (+ rid 2) 2) (poke32 (+ rid 4) 0) (poke64 (+ rid 8) *hwnd*)
    (u *register-raw-input* rid 1 16))
  (client-size)
  *hwnd*)

;;; ---- events ------------------------------------------------------------------------------------------------------
;;;
;;; The window's events, encoded as the Mac host encodes them (1004): type<<40 | y<<20 | x for the pointer, in
;;; pixels; type<<40 | keysym for a key.  1 press, 2 drag, 3 release (the left button); 5 a move with no button
;;; down; 6 a resize (h<<20 | w); 8 a key pressed, 9 released; 12 another button or the wheel (the X mask in bits
;;; 48 up); 13 the locked pointer's motion (dx and dy, 20 bits each, two's complement).

(defvar *events* (make-array 4096 :initial-element 0))
(defvar *ev-head* 0)
(defvar *ev-tail* 0)

(defun push-event (e)
  (when (< (- *ev-tail* *ev-head*) (length *events*))
    (setf (aref *events* (mod *ev-tail* (length *events*))) e)
    (incf *ev-tail*)))

(defun next-event ()
  (if (= *ev-head* *ev-tail*)
      0
      (prog1 (aref *events* (mod *ev-head* (length *events*))) (incf *ev-head*))))

(defun pointer-event (type x y)
  (push-event (logior (ash type 40) (ash (max 0 y) 20) (max 0 x))))

(defvar *ui-events* '() "Terms for SWIFT-PUMP (ui_events_next), oldest first.")

(defvar *debug* (let ((v (modus-int::%cli-getenv "VESSEL_DEBUG"))) (and v (plusp (length v))))
  "VESSEL_DEBUG set: each native event, and each WinRT step, logged.")

(defun ui-event (term)
  (when *debug* (format t "~&vessel-windows: event ~S~%" term))
  (setf *ui-events* (nconc *ui-events* (list term))))

(defvar *held* (make-hash-table) "Each key held: its virtual key -> the keysym its press sent.")
(defvar *shift* nil)
(defvar *ctrl* nil)
(defvar *buttons* 0 "The X mask of the buttons held.")
(defvar *accels* '() "(KEYSYM . ITEM): Ctrl+KEY chooses the menu ITEM, as Command+KEY on a Mac.")

(defun keysym (vk)
  (cond ((<= #x41 vk #x5A) (if *shift* vk (+ vk 32)))
        ((<= #x30 vk #x39) vk)
        ((= vk #x20) #x20)
        ((= vk #xBB) (if *shift* #x2B #x3D)) ((= vk #xBD) (if *shift* #x5F #x2D))
        ((= vk #xBC) #x2C) ((= vk #xBE) #x2E) ((= vk #xBF) #x2F) ((= vk #xBA) #x3B)
        ((= vk #xDE) #x27) ((= vk #xDB) #x5B) ((= vk #xDD) #x5D) ((= vk #xDC) #x5C) ((= vk #xC0) #x60)
        (t (modus-int::%ww-vk-keysym vk))))

(defun release-keys ()
  "Every key held, let go: the window stopped taking the keyboard, and its releases go elsewhere."
  (maphash (lambda (vk ks) (declare (ignore vk)) (push-event (logior (ash 9 40) ks))) *held*)
  (clrhash *held*)
  (setf *shift* nil *ctrl* nil))

(defun key (vk down repeat)
  (when (= vk 16) (setf *shift* down))
  (when (= vk 17) (setf *ctrl* down))
  (if down
      (unless repeat
        (let ((ks (keysym vk)))
          (when ks
            (let ((accel (and *ctrl* (assoc (if (<= #x41 vk #x5A) (+ vk 32) ks) *accels*))))
              (if accel
                  (ui-event (list :menu (cdr accel)))
                  (progn (setf (gethash vk *held*) ks)
                         (push-event (logior (ash 8 40) ks))))))))
      (let ((ks (gethash vk *held*)))
        (when ks
          (remhash vk *held*)
          (push-event (logior (ash 9 40) ks))))))

;;; THE POINTER, LOCKED (a game's mouse-look): hidden, held in the window (ClipCursor), and put back in the middle
;;; after each move, whose distance from the middle is the motion.  Wanted until let go; lost while the window is
;;; not in front, and taken again by a click in it.

(defvar *lock-wanted* nil)
(defvar *locked* nil)
(defvar *cursor* 0 "The pointer's shape over the scene, as last chosen.")

(defun client-to-screen (x y)
  (let ((p (point-area)))
    (poke32 p x) (poke32 (+ p 4) y)
    (c *map-points* *hwnd* 0 p 1)
    (values (s32 (peek32 p)) (s32 (peek32 (+ p 4))))))

(defun lock-pointer ()
  (client-size)
  (let ((r (rect-area)))
    (multiple-value-bind (x0 y0) (client-to-screen (floor *w* 2) (floor *h* 2))
      (poke32 r x0) (poke32 (+ r 4) y0) (poke32 (+ r 8) (+ x0 2)) (poke32 (+ r 12) (+ y0 2))
      (c *clip-cursor* r)))
  (poke64 (+ *page* #x40) 0)
  (u *set-cursor* 0)
  (setf *locked* t))

(defun unlock-pointer ()
  (when *locked*
    (c *clip-cursor* 0)
    (let ((cur (if (zerop *cursor*) *arrow* *cursor*)))
      (poke64 (+ *page* #x40) cur)
      (u *set-cursor* cur))
    (setf *locked* nil)))

;;; THE LOCKED POINTER'S MOTION is the mouse's own (raw input: WM_INPUT, read by the window procedure), not where the
;;; pointer is: while locked the pointer is pinned to the middle of the window (ClipCursor to a 2-pixel square) and
;;; hidden.  Measuring the pointer and putting it back raced the windows' thread, which records moves while this one
;;; draws: look-around ran too fast, then jumped.

(defvar *minimized* nil)
(defvar *on-quit* nil)
(defvar *snapshot-next* nil "The window a WM_APP+2 was posted to: read its view's next frame back (VESSEL_SNAPSHOT).")

(defun dropped (hdrop)
  (let ((n (logand (c *drag-query* hdrop #xFFFFFFFF 0 0) #xFFFFFFFF)))
    (dotimes (i n)
      (let* ((len (logand (c *drag-query* hdrop i 0 0) #xFFFFFFFF))
             (buf (+ *cb* #x8000)))
        (when (< len 4000)
          (c *drag-query* hdrop i buf (+ len 1))
          (ui-event (list :dropped 0 (modus-int::w32-wstr buf len))))))
    (c *drag-finish* hdrop)))

(defun handle-message (msg hwnd wp lp)
  (let* ((main (= hwnd *hwnd*))
         (other (and (not main) (gethash hwnd *by-hwnd*))))
    (cond
      ((and other (eq (ctl-kind (gethash other *ctls*)) :window) (member msg '(#x05 #x10)))
       (window-message other msg wp lp))
      ((and main (= msg #x05))                              ; WM_SIZE
       (if (= wp 1)
           (unless *minimized* (setf *minimized* t) (ui-event (list :appstate 0 "hidden")))
           (progn
             (when *minimized* (setf *minimized* nil) (ui-event (list :appstate 0 "shown")))
             (setf *w* (lo16 lp) *h* (hi16 lp))
             (push-event (logior (ash 6 40) (ash *h* 20) *w*)))))
      ((and main (= msg #x08))                              ; WM_KILLFOCUS
       (release-keys) (unlock-pointer))
      ((and main (= msg #x1C))                              ; WM_ACTIVATEAPP
       (if (zerop wp)
           (progn (release-keys) (unlock-pointer) (lb-mark :inactive) (ui-event (list :appstate 0 "inactive")))
           (progn (lb-mark :active) (ui-event (list :appstate 0 "active")))))
      ((and main (= msg #x10))                              ; WM_CLOSE
       (if *on-quit* (ui-event (list :quit 0)) (quit-now)))
      ((and main (<= #x100 msg #x105) (/= msg #x102) (/= msg #x103))
       (key (logand wp #xFF) (or (= msg #x100) (= msg #x104)) (logbitp 30 lp)))
      ((and main (<= #x200 msg #x20E))
       (let ((x (s16 lp)) (y (s16 (ash lp -16))))
         (case msg
           (#x200 (cond (*locked* nil)                     ; motion comes from WM_INPUT
                        ((logtest *buttons* 1) (pointer-event 2 x y))
                        (t (pointer-event 5 x y))))
           (#x201 (u *set-focus* *hwnd*)
                  (setf *buttons* (logior *buttons* 1))
                  (when (and *lock-wanted* (not *locked*)) (lock-pointer))
                  (pointer-event 1 x y))
           (#x202 (setf *buttons* (logand *buttons* (lognot 1))) (pointer-event 3 x y))
           ((#x204 #x205 #x207 #x208)
            (setf *buttons* (case msg (#x204 (logior *buttons* 4)) (#x205 (logand *buttons* (lognot 4)))
                              (#x207 (logior *buttons* 2)) (t (logand *buttons* (lognot 2)))))
            (push-event (logior (ash *buttons* 48) (ash 12 40) (ash (max 0 y) 20) (max 0 x))))
           (#x20A (if (logtest wp 8)
                      ;; WITH CONTROL: a pinch.  A precision touchpad's pinch comes to a Win32 window as Ctrl+wheel
                      ;; (and so does Ctrl and a mouse's wheel): a type 15 gesture, :pinch :changed, a tenth a notch
                      (let ((n (round (* 100 (s16 (ash wp -16))) 120)))
                        (push-event (logior (ash 15 40) (ash 1 36) (ash 0 32) (logand n #xFFFFFFFF))))
                      (let ((bit (if (plusp (s16 (ash wp -16))) 8 16)))
                        ;; the wheel's position is the screen's; the event is where the pointer is
                        (push-event (logior (ash (logior *buttons* bit) 48) (ash 12 40)))
                        (push-event (logior (ash *buttons* 48) (ash 12 40)))))))))
      ((and main (= msg #xFF))                              ; WM_INPUT: the mouse's own motion, recorded as dx dy
       (when *locked*
         (let ((dx (s32 wp)) (dy (s32 lp)))
           (unless (and (zerop dx) (zerop dy))
             (push-event (logior (ash 13 40) (ash (logand dy #xFFFFF) 20) (logand dx #xFFFFF)))))))
      ((= msg #x111)                                        ; WM_COMMAND
       (if (zerop lp)
           (let ((item (gethash (lo16 wp) *menu-ids*)))
             (when item (ui-event (list :menu item))))
           (let ((h (gethash lp *by-hwnd*)))
             (when (and h (zerop (hi16 wp)))               ; BN_CLICKED
               (ui-event (list :tap h))
               (u *set-focus* *hwnd*)))))
      ((= msg #x114)                                        ; WM_HSCROLL: a slider
       (let ((h (gethash lp *by-hwnd*)))
         (when h
           (let ((v (logand (c *send-message* lp #x400 0 0) #xFFFFFFFF)))   ; TBM_GETPOS
             (unless (eql v (ctl-value (gethash h *ctls*)))
               (setf (ctl-value (gethash h *ctls*)) v)
               (ui-event (list :slider h v)))))
         (when (= (lo16 wp) 8) (u *set-focus* *hwnd*))))   ; TB_ENDTRACK
      ((= msg #x233) (dropped wp))                         ; WM_DROPFILES
      ((= msg #x8001) (tray-message (lo16 lp)))
      ((= msg #x8015) (posted-answer wp lp))             ; WM_APP+21: a UI-POST's answer
      ((= msg #x8002)
       (when *debug* (format t "~&vessel-windows: snapshot of ~D asked for~%" hwnd))
       (setf *snapshot-next* hwnd)))))    ; WM_APP+2: a snapshot of that window's next frame

(defun pump ()
  "Run the window's messages, then turn what they said into events."
  (let ((msg (msg-area)))
    (loop while (/= 0 (logand (c *peek-message* msg 0 0 0 1) #xFFFFFFFF))
          do (c *translate-message* msg)
             (c *dispatch-message* msg)))
  (dolist (page *pages*)
    (dolist (e (take-ring page))
      (apply #'handle-message e))))

(defun quit-now ()
  (tray-remove)
  (modus-int::%cli-exit 0))

;;; ---- the frame clock ---------------------------------------------------------------------------------------------

(defvar *timers* '() "(INTERNAL-TIME . FN): run once the time has come.")

(defun after (seconds fn)
  (push (cons (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))) fn) *timers*))

(defun run-timers ()
  (let ((now (get-internal-real-time)) (due '()))
    (setf *timers* (remove-if (lambda (tm) (when (<= (car tm) now) (push (cdr tm) due) t)) *timers*))
    (dolist (fn due) (ignore-errors (funcall fn)))))

(defvar *speaker* nil "The speaker: :WASAPI, or W32-AUDIO-OPEN's waveOut speaker, once opened.")

(defun feed-speaker ()
  "Top the speaker up (sound.lisp's SOUND-PUMP), if the app has one."
  (let ((f (find-symbol "SOUND-PUMP" "KILN-SOUND")))
    (when (and f (fboundp f) *speaker*) (ignore-errors (funcall f)))))

(defvar *stats-started* nil)
(defvar *stats* (let ((v (modus-int::%cli-getenv "VESSEL_STATS"))) (and v (plusp (length v)))))
(defvar *stat-frames* 0)
(defvar *stat-longest* 0 "The most time one frame's work took this second (frame clock to frame clock), internal time units.")
(defvar *stat-last-tick* nil)
(defvar *stat-gcs* nil "The collection count at the start of this second.")
(defvar *stat-wait* 0 "The longest wait for the frame clock (DwmFlush) this second.")
(defvar *stat-presented* nil "When the last frame was presented.")
(defvar *stat-work* 0 "The longest frame's work this second: from the frame clock to just before Present.")
(defvar *stat-repeats* 0 "Refreshes this second that showed the frame before again (DXGI's frame statistics).")
(defvar *stat-dxgi* nil "The last (present-count refresh-count) DXGI reported.")
(defvar *stat-late* 0 "Frames this second that came more than 20 ms after the one before: a refresh missed.")
(defvar *stat-early* 0 "Frames this second that came less than 10 ms after the one before: two in one refresh.")
(defvar *stat-pump* 0 "The longest run of the window's messages this second.")

(defun wait-frame (ms)
  "Until the next composition (the display's refresh): 0.  Minimized, there is none; MS milliseconds then."
  (let ((now (get-internal-real-time)))
    (when *stat-last-tick* (setf *stat-longest* (max *stat-longest* (- now *stat-last-tick*))))
    (unless *stats-started* (setf *stats-started* t) (after 1 #'stats-report)))
  (let ((t0 (get-internal-real-time)))
    (cond ((or *minimized* (/= 0 (logand (c *is-iconic* *hwnd*) #xFFFFFFFF)))
           (gc-safe *sleep* (max 1 ms)))
          ((and *flip* (plusp *frame-wait*))
           ;; the swap chain says when: waited on in 8 ms slices (100 ms at most), the speaker fed between them, so a
           ;; frame that comes late -- DWM can hold one for two or three refreshes -- does not run the speaker dry
           (loop repeat 12
                 until (zerop (logand (gc-safe *wait-object* *frame-wait* 8 1) #xFFFFFFFF))   ; WAIT_OBJECT_0
                 do (feed-speaker)))
          (*flip* nil)
          (t (gc-safe *dwm-flush*)))
    (let ((t1 (get-internal-real-time)))
      (setf *stat-wait* (max *stat-wait* (- t1 t0)))
      (pump)
      (setf *stat-pump* (max *stat-pump* (- (get-internal-real-time) t1)))))
  (setf *stat-last-tick* (get-internal-real-time))
  (when *loopback* (loopback-poll))
  (run-timers)
  0)

;;; ---- controls ------------------------------------------------------------------------------------------------------


(defun new-handle (ctl)
  (let ((h *next-handle*))
    (incf *next-handle*)
    (setf (gethash h *ctls*) ctl)
    (unless (zerop (ctl-hwnd ctl)) (setf (gethash (ctl-hwnd ctl) *by-hwnd*) h))
    h))

(defun ctl-of (h) (or (gethash h *ctls*) (error "vessel-windows: no handle ~S" h)))

(defun child (class text style x y w h)
  (let ((hwnd (u *create-window* 0 (wide1 class) (wide2 text) (logior #x50000000 style)   ; WS_CHILD|WS_VISIBLE
                 (px x) (px y) (px w) (px h) *hwnd* 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no ~A" class))
    (c *send-message* hwnd #x30 *font* 1)                                                  ; WM_SETFONT
    hwnd))

(defun label-new (addr n x y w h)
  (new-handle (make-ctl :kind :label :hwnd (child "STATIC" (utf8-text addr n) #x80 x y w h))))   ; SS_NOPREFIX

(defun button-new (addr n x y w h)
  (new-handle (make-ctl :kind :button :hwnd (child "BUTTON" (utf8-text addr n) 0 x y w h))))

(defun slider-new (value x y w h)
  (let ((hwnd (child "msctls_trackbar32" "" #x10 x y w h)))                               ; TBS_NOTICKS
    (c *send-message* hwnd #x406 1 (ash 100 16))                                           ; TBM_SETRANGE 0..100
    (c *send-message* hwnd #x405 1 value)                                                  ; TBM_SETPOS
    (new-handle (make-ctl :kind :slider :hwnd hwnd :value value))))

(defun panel-new (x y w h radius)
  (let ((hwnd (u *create-window* 0 (wide1 "VesselPanel") (wide2 "") #x56000000            ; CHILD VISIBLE CLIP*
                 (px x) (px y) (px w) (px h) *hwnd* 0 *hinst* 0)))
    (when (zerop hwnd) (error "vessel-windows: no panel"))
    (when (plusp radius)
      (u *set-window-rgn* hwnd (c *create-round-rgn* 0 0 (+ (px w) 1) (+ (px h) 1) (px (* 2 radius))
                                  (px (* 2 radius)))
         1))
    (new-handle (make-ctl :kind :panel :hwnd hwnd))))

(defun set-text (h addr n)
  "A control's text, set on the windows' thread without waiting for it (SetWindowTextW from another thread is a
   SendMessage, which waits until that thread answers)."
  (let ((ctl (ctl-of h)) (text (utf8-text addr n)))
    (unless (zerop (ctl-hwnd ctl))
      (ui-post *set-window-text* (list (ctl-hwnd ctl) (lambda (mem) (wide-at mem text)))
               (lambda (r mem) r mem) (* 4 (+ 1 (length text)))))
    0))

(defun set-value (h v)
  (let ((ctl (ctl-of h)))
    (when (eq (ctl-kind ctl) :slider)
      (setf (ctl-value ctl) v)
      (c *send-message* (ctl-hwnd ctl) #x405 1 v))
    0))

;;; ---- menus, the notification area, alerts --------------------------------------------------------------------------

(defvar *menu-bar* 0)
(defvar *next-id* 100)

(defun menu-new (addr n in-bar)
  (let ((popup (c *create-popup-menu*)))
    (when (/= in-bar 0)
      (when (zerop *menu-bar*) (setf *menu-bar* (c *create-menu*)))
      (c *append-menu* *menu-bar* #x10 popup (wide1 (utf8-text addr n)))                  ; MF_POPUP
      (u *set-menu* *hwnd* *menu-bar*)
      (u *draw-menu-bar* *hwnd*))
    (new-handle (make-ctl :kind :menu :data popup))))

(defun key-label (key)
  (cond ((string= key "=") "Ctrl+=") ((string= key "-") "Ctrl+-") (t (format nil "Ctrl+~:@(~A~)" key))))

(defun menu-item-new (menu addr n kaddr kn)
  (let* ((popup (ctl-data (ctl-of menu)))
         (title (utf8-text addr n)) (key (utf8-text kaddr kn))
         (id *next-id*))
    (incf *next-id*)
    (c *append-menu* popup 0 id (wide1 (if (plusp (length key)) (format nil "~A~C~A" title #\Tab (key-label key)) title)))
    (let ((h (new-handle (make-ctl :kind :item :data (list popup id)))))
      (setf (gethash id *menu-ids*) h)
      (when (plusp (length key)) (push (cons (char-code (char-downcase (char key 0))) h) *accels*))
      h)))

(defun menu-item-set (item checked enabled)
  (destructuring-bind (popup id) (ctl-data (ctl-of item))
    (c *check-menu-item* popup id (if (/= checked 0) 8 0))                                 ; MF_CHECKED
    (c *enable-menu-item* popup id (if (/= enabled 0) 0 1))                                ; MF_GRAYED
    (u *draw-menu-bar* *hwnd*)
    0))

;;; The notification area: one icon, made by the first status item or notification.  NOTIFYICONDATAW (976 bytes):
;;; +0 cbSize  +8 hWnd  +16 uID  +20 uFlags  +24 uCallbackMessage  +32 hIcon  +40 szTip[128]  +296 dwState
;;; +300 dwStateMask  +304 szInfo[256]  +816 uVersion  +820 szInfoTitle[64]  +948 dwInfoFlags  +952 guidItem
;;; +968 hBalloonIcon.  Version 4: the callback's lParam is the event (NIN_SELECT, WM_CONTEXTMENU,
;;; NIN_BALLOONUSERCLICK) in its low word.

(defvar *tray* nil)
(defvar *tray-tip* "Hello")
(defvar *status-menu* nil "The status item's menu (a popup), shown on a click.")
(defvar *status-item* nil)
(defvar *last-notification* 0)

(defun nid (flags)
  (let ((a (nid-area)))
    (zero a 976)
    (poke32 a 976)
    (poke64 (+ a 8) *hwnd*)
    (poke32 (+ a 16) 1)
    (poke32 (+ a 20) flags)
    (poke32 (+ a 24) #x8001)
    (poke64 (+ a 32) (c *load-icon* 0 32512))
    (let ((tip *tray-tip*)) (wide-at (+ a 40) (subseq tip 0 (min 127 (length tip)))))
    a))

(defun tray-add ()
  (unless *tray*
    (when (zerop (logand (c *notify-icon* 0 (nid 7)) #xFFFFFFFF))                         ; NIM_ADD: msg icon tip
      (format t "~&vessel-windows: the notification area refused the icon~%"))
    (let ((a (nid 0))) (poke32 (+ a 816) 4) (c *notify-icon* 4 a))                         ; NIM_SETVERSION 4
    (setf *tray* t)))

(defun tray-remove ()
  (when *tray* (c *notify-icon* 2 (nid 0)) (setf *tray* nil)))

(defun tray-tip (text)
  "The status item's text, as the icon's tooltip.  Shell_NotifyIconW waits on Explorer (another process), and the
   app sets this twice a second (hello3d's frame rate): so it is sent from the windows' thread, not waited for here,
   and only when the text changed."
  (unless (equal text *tray-tip*)
    (setf *tray-tip* text)
    (when *tray*
      (nid #x84)                                                                           ; NIM_MODIFY: tip, show tip
      (ui-post *notify-icon* (list 1 (lambda (mem) (c *memcpy* mem (nid-area) 976) mem)) (lambda (r mem) r mem) 976))))

(defun tray-balloon (title body)
  (tray-add)
  (let ((a (nid #x10)))                                                                    ; NIF_INFO
    (wide-at (+ a 304) (subseq body 0 (min 255 (length body))))
    (wide-at (+ a 820) (subseq title 0 (min 63 (length title))))
    (poke32 (+ a 948) 1)                                                                   ; NIIF_INFO
    (if (zerop (logand (c *notify-icon* 1 a) #xFFFFFFFF))
        (format t "~&vessel-windows: the notification ~S was refused~%" title)
        (format t "~&vessel-windows: notified: ~A~%" title))))

(defun tray-message (what)
  (case what
    ((#x400 #x7B #x202 #x205)                                ; NIN_SELECT, WM_CONTEXTMENU, a click
     (cond (*status-menu*
            (let ((p (point-area)))
              (c *get-cursor-pos* p)
              (u *set-foreground* *hwnd*)
              ;; a menu is a modal loop: on the windows' thread, answered later
              (ui-post *track-popup-menu* (list *status-menu* #x102 (s32 (peek32 p)) (s32 (peek32 (+ p 4))) 0 *hwnd* 0)
                       (lambda (id mem)
                         (declare (ignore mem))
                         (let ((item (gethash (logand id #xFFFFFFFF) *menu-ids*)))
                           (when item (ui-event (list :menu item))))))))
           (*status-item* (ui-event (list :status *status-item*)))))
    (#x405 (ui-event (list :notified *last-notification*))))) ; NIN_BALLOONUSERCLICK

(defun alert-present (h)
  (let* ((ctl (ctl-of h)) (data (ctl-data ctl))
         (actions (length (third data)))
         (flags (cond ((<= actions 1) #x40) ((= actions 2) #x41) (t #x43))))  ; ICONINFORMATION | OK / OKCANCEL / YESNOCANCEL
    (unlock-pointer)
    ;; a message box is a modal loop: on the windows' thread, its strings in the request's own memory, answered later
    (ui-post *message-box* (list *hwnd*
                                 (lambda (mem) (wide-at mem (second data)))
                                 (lambda (mem) (wide-at (+ mem #x1000) (first data)))
                                 flags)
             (lambda (r mem)
               (declare (ignore mem))
               (let* ((r (logand r #xFFFFFFFF))
                      (index (cond ((<= actions 1) 0)
                                   ((= actions 2) (if (= r 1) 1 0))
                                   (t (case r (6 (- actions 1)) (7 1) (t 0))))))
                 (ui-event (list :alert h index))))
             #x1F00)
    0))

;;; ---- the GPU: Direct3D 11 ------------------------------------------------------------------------------------------
;;;
;;; gpu.lisp's calls, on one device and a swap chain per view.  A view is made by gpu_view_new and drawn in the
;;; window it is in: the app's window for the first one, the window it is added to (ui_add) for any other -- hello3d's
;;; Map.  Shaders read their vertices themselves (Metal's vertex pulling): a buffer at INDEX for the vertex function is
;;; a raw shader resource at t<INDEX> in the vertex stage; constants at INDEX are the cbuffer b<INDEX> in both stages; a
;;; texture at INDEX is t<8+INDEX> in the pixel stage and its sampler s<INDEX>; the draw's first vertex is in b13, to
;;; add to SV_VertexID (FIRST-VERTEX).  A swap chain is the blit model (DXGI_SWAP_EFFECT_DISCARD), so a window's
;;; child windows -- the controls -- draw over it as they do over GDI.  Faces are counter-clockwise in front, depth is
;;; 0 near to 1 far, and matrices are column-major: Metal's conventions, so the same Lisp drives both.
;;;
;;; Vtable slots used: ID3D11Device 3 CreateBuffer, 5 CreateTexture2D, 7 CreateShaderResourceView, 9
;;; CreateRenderTargetView, 10 CreateDepthStencilView, 12 CreateVertexShader, 15 CreatePixelShader, 20
;;; CreateBlendState, 21 CreateDepthStencilState, 22 CreateRasterizerState, 23 CreateSamplerState.
;;; ID3D11DeviceContext 7 VSSetConstantBuffers, 8 PSSetShaderResources, 9 PSSetShader, 10 PSSetSamplers, 11
;;; VSSetShader, 12 DrawIndexed, 13 Draw, 14 Map, 15 Unmap, 16 PSSetConstantBuffers, 17 IASetInputLayout, 19
;;; IASetIndexBuffer, 24 IASetPrimitiveTopology, 25 VSSetShaderResources, 33 OMSetRenderTargets, 35
;;; OMSetBlendState, 36 OMSetDepthStencilState, 43 RSSetState, 44 RSSetViewports, 47 CopyResource, 48
;;; UpdateSubresource, 50 ClearRenderTargetView, 53 ClearDepthStencilView, 54 GenerateMips.  IDXGIDevice 7
;;; GetAdapter; IDXGIObject 6 GetParent; IDXGIFactory 8 MakeWindowAssociation, 10 CreateSwapChain.  IDXGISwapChain
;;; 8 Present, 9 GetBuffer, 13 ResizeBuffers.  ID3DBlob 3 GetBufferPointer, 4 GetBufferSize.

(defvar *dev* 0)
(defvar *ctx* 0)
(defvar *factory* 0)
(defvar *rasters* (make-array 3 :initial-element 0))
(defvar *depths* (make-array 3 :initial-element 0))
(defvar *blend* 0)
(defvar *sampler* 0)
(defvar *cbufs* (make-array 16 :initial-element 0))
(defvar *first-vertex* nil "The first vertex b13 holds now (NIL: not written this pass; FIRST-VERTEX).")

(defvar *flip* (not (equal (modus-int::%cli-getenv "VESSEL_FLIP") "0"))
  "The FLIP MODEL (DXGI_SWAP_EFFECT_FLIP_DISCARD), presented on the refresh (Present(1)) with one frame in flight:
   DWM shows each frame exactly once.  The blit model (VESSEL_FLIP=0) copies a frame whenever it is presented and
   DWM composes on its own clock, so the two drift and about one refresh a second shows the frame before again --
   a stutter at a steady 60 fps.  With the flip model Present is the frame clock and DwmFlush is not waited on.")

(defvar *frame-wait* 0 "The main view's frame latency waitable object (flip model): the frame clock waits on it.")

(defstruct view (hwnd 0) (swap 0) (rtv 0) (dsv 0) (depth 0) (w 0) (h 0) (print nil))

(defvar *main-view* nil "The first view: the app's window's.")

(defun out0 () (poke64 (cell 0) 0) (cell 0))

(defun created (hr what)
  (check hr what)
  (peek64 (cell 0)))

(defun guid-at (a text) (modus-int::w32-put-guid a text) a)

(defun create-states ()
  ;; rasterizer: solid, cull none / back / front, counter-clockwise in front, depth clip on
  (dotimes (i 3)
    (let ((d (area 0)))
      (zero d 40)
      (poke32 d 3) (poke32 (+ d 4) (aref #(1 3 2) i)) (poke32 (+ d 8) 1) (poke32 (+ d 24) 1)
      (setf (aref *rasters* i) (created (m *dev* 22 d (out0)) "CreateRasterizerState"))))
  ;; depth: off / test and write (less) / test only
  (dotimes (i 3)
    (let ((d (area 0)))
      (zero d 52)
      (unless (zerop i) (poke32 d 1) (poke32 (+ d 4) (if (= i 1) 1 0)))
      (poke32 (+ d 8) (if (zerop i) 8 2))                ; ALWAYS when off (unused), else LESS
      (poke8 (+ d 16) #xFF) (poke8 (+ d 17) #xFF)
      (dolist (face '(20 36))                            ; stencil off, but its ops must be valid: KEEP, ALWAYS
        (poke32 (+ d face) 1) (poke32 (+ d face 4) 1) (poke32 (+ d face 8) 1) (poke32 (+ d face 12) 8))
      (setf (aref *depths* i) (created (m *dev* 21 d (out0)) "CreateDepthStencilState"))))
  ;; blend: source over
  (let ((d (area 0)))
    (zero d 264)
    (dotimes (k 8)                                       ; every target alike
      (let ((rt (+ d 8 (* 32 k))))
        (poke32 rt 1) (poke32 (+ rt 4) 5) (poke32 (+ rt 8) 6) (poke32 (+ rt 12) 1)
        (poke32 (+ rt 16) 2) (poke32 (+ rt 20) 6) (poke32 (+ rt 24) 1) (poke8 (+ rt 28) #xF)))
    (setf *blend* (created (m *dev* 20 d (out0)) "CreateBlendState")))
  ;; sampler: anisotropic, mipmapped, repeating
  (let ((d (area 0)))
    (zero d 52)
    (poke32 d #x55) (poke32 (+ d 4) 1) (poke32 (+ d 8) 1) (poke32 (+ d 12) 1)
    (poke32 (+ d 20) 8) (poke32 (+ d 24) 1) (poke32 (+ d 48) #x7F7FFFFF)
    (setf *sampler* (created (m *dev* 23 d (out0)) "CreateSamplerState"))))

(defun gpu-init ()
  (when (zerop *dev*)
    (dolist (driver '(1 5))                      ; the hardware, else WARP
      (dotimes (i 3) (poke64 (cell i) 0))
      (let ((hr (c *d3d11-create-device* 0 driver 0 0 0 0 7 (cell 0) (cell 1) (cell 2))))
        (unless (failed-p hr)
          (setf *dev* (peek64 (cell 0)) *ctx* (peek64 (cell 2)))
          (format t "~&vessel-windows: Direct3D 11 (~:[hardware~;WARP, in software~], feature level ~X)~%"
                  (= driver 5) (peek32 (cell 1)))
          (return))))
    (when (zerop *dev*) (error "vessel-windows: no Direct3D 11 device"))
    ;; the factory that made the device's adapter makes its swap chains
    (let* ((dxgi (created (m *dev* 0 (guid-at (area 1) "54ec77fa-1377-44e6-8c32-88fd5f44c84c") (out0)) "IDXGIDevice"))
           (adapter (created (m dxgi 7 (out0)) "GetAdapter")))
      (setf *factory* (created (m adapter 6 (guid-at (area 1) "7b7166ec-21c7-44ae-b21a-c9ae321ae369") (out0))
                               "IDXGIFactory"))
      (release adapter) (release dxgi))
    ;; one frame in flight: a Present on the refresh waits for the frame before it, not three behind
    (let ((dxgi1 (created (m *dev* 0 (guid-at (area 1) "77db970f-6276-48ba-ba28-070143b4392c") (out0)) "IDXGIDevice1")))
      (m dxgi1 12 1)                              ; SetMaximumFrameLatency(1)
      (release dxgi1))
    (create-states)))

(defun hwnd-size (hwnd)
  (let ((r (rect-area)))
    (c *get-client-rect* hwnd r)
    (values (max 0 (s32 (peek32 (+ r 8)))) (max 0 (s32 (peek32 (+ r 12)))))))

(defun make-targets (v)
  (let ((bb (created (m (view-swap v) 9 0 (guid-at (area 1) "6f15aaf2-d208-4e89-9ab4-489535d34f9c") (out0)) "GetBuffer")))
    (setf (view-rtv v) (created (m *dev* 9 bb 0 (out0)) "CreateRenderTargetView"))
    (release bb))
  (multiple-value-bind (w h) (hwnd-size (view-hwnd v))
    (setf (view-w v) w (view-h v) h))
  (let ((d (area 0)))
    (zero d 44)
    (poke32 d (max 1 (view-w v))) (poke32 (+ d 4) (max 1 (view-h v))) (poke32 (+ d 8) 1) (poke32 (+ d 12) 1)
    (poke32 (+ d 16) 45) (poke32 (+ d 20) 1) (poke32 (+ d 32) #x40)                         ; D24S8, DEPTH_STENCIL
    (setf (view-depth v) (created (m *dev* 5 d 0 (out0)) "CreateTexture2D (depth)"))
    (setf (view-dsv v) (created (m *dev* 10 (view-depth v) 0 (out0)) "CreateDepthStencilView"))))

(defun view-attach (v hwnd)
  "V drawn in the window HWND, filling it."
  (setf (view-hwnd v) hwnd)
  (let ((d (area 2)))
    (zero d 72)
    (poke32 (+ d 16) 28)                         ; R8G8B8A8_UNORM; width and height 0: the window's
    (poke32 (+ d 28) 1)                          ; one sample
    (poke32 (+ d 36) #x20)                       ; RENDER_TARGET_OUTPUT
    (poke32 (+ d 40) (if *flip* 2 1))            ; buffers: two to flip between, or one to blit
    (poke64 (+ d 48) hwnd)
    (poke32 (+ d 56) 1)                          ; windowed
    (poke32 (+ d 60) (if *flip* 4 0))            ; FLIP_DISCARD, or DISCARD (the blit model)
    (poke32 (+ d 64) (if *flip* #x40 0))         ; FRAME_LATENCY_WAITABLE_OBJECT
    (setf (view-swap v) (created (m *factory* 10 *dev* d (out0)) "CreateSwapChain")))
  (m *factory* 8 hwnd 2)                         ; no Alt+Enter full screen
  ;; THE FRAME CLOCK, with the flip model: the swap chain's waitable object, signalled when a new frame may begin --
  ;; in step with the display, so frames start evenly spaced and a game's elapsed time per frame is steady.  Blocking
  ;; in Present instead starts each frame whenever Present happens to return, a few milliseconds either way.
  (when (and *flip* (= hwnd *hwnd*))
    (let ((sc2 (created (m (view-swap v) 0 (guid-at (area 1) "a8be2ac4-199f-4946-b331-79599fb98de7") (out0))
                        "IDXGISwapChain2")))
      (m sc2 31 1)                                 ; SetMaximumFrameLatency(1)
      (setf *frame-wait* (m sc2 33))              ; GetFrameLatencyWaitableObject
      (release sc2)))
  (make-targets v))

(defun resize-targets (v)
  (m *ctx* 33 0 0 0)                              ; OMSetRenderTargets(0, NULL, NULL)
  (setf (view-rtv v) (release (view-rtv v)) (view-dsv v) (release (view-dsv v)) (view-depth v) (release (view-depth v)))
  (check (m (view-swap v) 13 0 0 0 0 (if *flip* #x40 0)) "ResizeBuffers")
  (make-targets v))

(defun gpu-handle (kind &rest data) (new-handle (make-ctl :kind kind :data data)))
(defun gpu-data (h) (ctl-data (ctl-of h)))
(defun view-of (h) (first (gpu-data h)))

(defun view-new ()
  (gpu-init)
  (let ((v (make-view)))
    (unless *main-view* (setf *main-view* v) (view-attach v *hwnd*))
    (gpu-handle :view v)))

(defun cbuf (index)
  (or (let ((b (aref *cbufs* index))) (and (plusp b) b))
      (let ((d (area 0)))
        (zero d 24)
        (poke32 d 4096) (poke32 (+ d 4) 2) (poke32 (+ d 8) 4) (poke32 (+ d 12) #x10000)   ; DYNAMIC, CONSTANT, WRITE
        (setf (aref *cbufs* index) (created (m *dev* 3 d 0 (out0)) "CreateBuffer (constants)")))))

(defun ptr-cell (i v) (poke64 (cell i) v) (cell i))

(defun pass-begin (h clear)
  (let ((v (view-of h)))
    (when (and *debug* (not (eq v *main-view*)) (< (mod *frames* 120) 1))
      (format t "~&vessel-windows: a pass on the view in ~D, ~S~%" (view-hwnd v) (multiple-value-list (hwnd-size (view-hwnd v)))))
    (when (zerop (view-hwnd v)) (return-from pass-begin 0))
    (multiple-value-bind (w hh) (hwnd-size (view-hwnd v))
      (when (or (< w 1) (< hh 1) (/= 0 (logand (c *is-iconic* (view-hwnd v)) #xFFFFFFFF)))
        (return-from pass-begin 0))
      (unless (and (= w (view-w v)) (= hh (view-h v))) (resize-targets v)))
    (m *ctx* 33 1 (ptr-cell 4 (view-rtv v)) (view-dsv v))
    (let ((vp (area 3)))
      (poke32 vp 0) (poke32 (+ vp 4) 0) (poke32 (+ vp 8) (fbits (view-w v))) (poke32 (+ vp 12) (fbits (view-h v)))
      (poke32 (+ vp 16) 0) (poke32 (+ vp 20) (fbits 1.0))
      (m *ctx* 44 1 vp))
    (let ((col (area 3)))
      (poke32 col (fbits (/ (ldb (byte 8 16) clear) 255.0)))
      (poke32 (+ col 4) (fbits (/ (ldb (byte 8 8) clear) 255.0)))
      (poke32 (+ col 8) (fbits (/ (ldb (byte 8 0) clear) 255.0)))
      (poke32 (+ col 12) (fbits 1.0))
      (m *ctx* 50 (view-rtv v) col))
    (m *ctx* 53 (view-dsv v) 1 (fbits 1.0) 0)     ; ClearDepthStencilView(DEPTH, 1.0, 0)
    (m *ctx* 43 (aref *rasters* 0))
    (m *ctx* 36 (aref *depths* 1) 0)
    (m *ctx* 17 0)
    (dotimes (i 4) (m *ctx* 10 i 1 (ptr-cell 5 *sampler*)))
    (setf *first-vertex* nil)
    (first-vertex 0)
    h))

;;; READING A FRAME BACK: the back buffer, as drawn and before it is shown, copied to a staging texture the CPU can
;;; read.  For a snapshot (VESSEL_SNAPSHOT: what was drawn, whether or not anything can see the screen) and for
;;; printing.

(defun read-back (v fn)
  "Call FN with (DATA PITCH W H): V's back buffer, mapped for reading (RGBA, PITCH bytes a row)."
  (let* ((bb (created (m (view-swap v) 9 0 (guid-at (area 1) "6f15aaf2-d208-4e89-9ab4-489535d34f9c") (out0)) "GetBuffer"))
         (d (area 0)))
    (zero d 44)
    (poke32 d (view-w v)) (poke32 (+ d 4) (view-h v)) (poke32 (+ d 8) 1) (poke32 (+ d 12) 1) (poke32 (+ d 16) 28)
    (poke32 (+ d 20) 1) (poke32 (+ d 28) 3) (poke32 (+ d 36) #x20000)              ; STAGING, CPU READ
    (let ((st (created (m *dev* 5 d 0 (out0)) "CreateTexture2D (staging)")))
      (unwind-protect
           (progn
             (m *ctx* 47 st bb)                                                    ; CopyResource
             (let ((mapped (area 3)))
               (check (m *ctx* 14 st 0 1 0 mapped) "Map (read)")
               (let ((data (peek64 mapped)) (pitch (peek32 (+ mapped 8))))
                 (unwind-protect (funcall fn data pitch (view-w v) (view-h v))
                   (m *ctx* 15 st 0)))))
        (release st)
        (release bb)))))

(defvar *frames* 0)
(defvar *snapshot* (let ((p (modus-int::%cli-getenv "VESSEL_SNAPSHOT"))) (and p (plusp (length p)) p))
  "VESSEL_SNAPSHOT=PATH: the app's view's 120th frame, and the next one after each WM_APP+2 posted to the window,
   read back from the GPU and written to PATH as a PPM (a quarter of the size; VESSEL_SNAPSHOT_STEP) -- what was
   drawn, whether or not anything can see the screen.")

(defvar *snapshot-step* (let ((v (ignore-errors (parse-integer (modus-int::%cli-getenv "VESSEL_SNAPSHOT_STEP")))))
                          (if (and v (plusp v)) v 4))
  "VESSEL_SNAPSHOT_STEP: every how many pixels a snapshot keeps (4; 1 for the whole picture).")

(defun snapshot (v path)
  "V's back buffer, written to PATH as a PPM (every *SNAPSHOT-STEP*th pixel)."
  (read-back v
    (lambda (data pitch sw sh)
      (let* ((step *snapshot-step*) (w (floor sw step)) (h (floor sh step))
             (header (format nil "P6 ~D ~D 255~C" w h #\Newline))
             (n (length header))
             (file (make-array (+ n (* 3 w h)) :element-type '(unsigned-byte 8))))
        (loop for ch across header for i from 0 do (setf (aref file i) (char-code ch)))
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (+ data (* step y pitch) (* 4 step x))) (o (+ n (* 3 (+ x (* y w))))))
              (setf (aref file o) (peek8 p) (aref file (+ o 1)) (peek8 (+ p 1)) (aref file (+ o 2)) (peek8 (+ p 2))))))
        ;; one WRITE-SEQUENCE, header and pixels together: on modus a WRITE-BYTE before a WRITE-SEQUENCE lands after
        ;; it in the file (the bytes wait in a buffer the sequence goes around)
        (with-open-file (f path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence file f))
        (format t "~&vessel-windows: frame ~D written to ~A (~Dx~D)~%" *frames* path w h)))))

;;; PRINTING (gpu_view_print): the view's next frame, read back, to the print dialog's printer -- scaled to fit the
;;; page, centred, as a 32-bit DIB whose bitfields say R G B are its bytes 0 1 2, so the GPU's RGBA goes as it is.
;;; VESSEL_PRINT_TO=PATH prints to "Microsoft Print to PDF" into PATH with no dialog (a test that uses no paper).

(defun print-pixels (bits w h)
  (let* ((to (let ((p (modus-int::%cli-getenv "VESSEL_PRINT_TO"))) (and p (plusp (length p)) p)))
         (pd (area 0)) (dc 0))
    (if to
        (setf dc (c *create-dc* (wide1 "WINSPOOL") (wide2 "Microsoft Print to PDF") 0 0))
        (progn
          (zero pd 120)
          (poke32 pd 120) (poke64 (+ pd 8) *hwnd*)
          (poke32 (+ pd 40) #x10C)                 ; PD_RETURNDC | PD_NOPAGENUMS | PD_NOSELECTION
          (poke16 (+ pd 52) 1)
          (unlock-pointer)
          (when (/= 0 (logand (c *print-dlg* pd) #xFFFFFFFF))
            (setf dc (peek64 (+ pd 32))))
          (dolist (off '(16 24)) (unless (zerop (peek64 (+ pd off))) (c *global-free* (peek64 (+ pd off)))))))
    (if (zerop dc)
        (format t "~&vessel-windows: not printed~%")
        (let* ((pw (s32 (c *get-device-caps* dc 8))) (ph (s32 (c *get-device-caps* dc 10)))
               (k (min (/ pw w) (/ ph h))) (dw (floor (* k w))) (dh (floor (* k h)))
               (di (area 1)) (bmi (area 2)))
          (zero di 40)
          (poke32 di 40) (poke64 (+ di 8) (wide1 "Hello 3D"))
          (when to (poke64 (+ di 16) (wide2 to)))
          (zero bmi 52)
          (poke32 bmi 40) (poke32 (+ bmi 4) w) (poke32 (+ bmi 8) (- h)) (poke16 (+ bmi 12) 1) (poke16 (+ bmi 14) 32)
          (poke32 (+ bmi 16) 3)                    ; BI_BITFIELDS
          (poke32 (+ bmi 40) #xFF) (poke32 (+ bmi 44) #xFF00) (poke32 (+ bmi 48) #xFF0000)
          (if (<= (s32 (c *start-doc* dc di)) 0)
              (format t "~&vessel-windows: the printer would not start the document~%")
              (progn
                (c *start-page* dc)
                (c *set-stretch-mode* dc 4) (c *set-brush-org* dc 0 0 0)   ; HALFTONE
                (c *stretch-dibits* dc (floor (- pw dw) 2) (floor (- ph dh) 2) dw dh 0 0 w h bits bmi 0 #x00CC0020)
                (c *end-page* dc)
                (c *end-doc* dc)
                (format t "~&vessel-windows: printed ~Dx~D~@[ to ~A~]~%" w h to)))
          (c *delete-dc* dc)))))

(defun print-view (v)
  (let ((bits 0) (size 0) (w 0) (h 0))
    (read-back v (lambda (data pitch sw sh)
                   (setf w sw h sh size (* 4 sw sh) bits (modus-int::w32-alloc size))
                   (dotimes (y sh) (c *memcpy* (+ bits (* y 4 sw)) (+ data (* y pitch)) (* 4 sw)))))
    (unwind-protect (print-pixels bits w h)
      (modus-int::w32-free bits size))))

(defun pass-end (h)
  (let ((v (view-of h)))
    (when (eq v *main-view*)
      (incf *frames*) (incf *stat-frames*)
      (let ((now (get-internal-real-time)))
        (when *stat-presented*
          (let ((ms (/ (* 1000 (- now *stat-presented*)) internal-time-units-per-second)))
            (cond ((> ms 20) (incf *stat-late*)) ((< ms 10) (incf *stat-early*)))))
        (setf *stat-presented* now)))
    (when (and *snapshot* (or (and (eq v *main-view*) (= *frames* 120)) (eql *snapshot-next* (view-hwnd v))))
      (setf *snapshot-next* nil)
      (handler-case (snapshot v *snapshot*)
        (error (e) (format t "~&vessel-windows: snapshot: ~A~%" e))))
    (when (view-print v)
      (setf (view-print v) nil)
      (handler-case (print-view v)
        (error (e) (format t "~&vessel-windows: print: ~A~%" e))))
    (when (and (eq v *main-view*) *stat-last-tick*)
      (setf *stat-work* (max *stat-work* (- (get-internal-real-time) *stat-last-tick*))))
    (when (and *flip* (eq v *main-view*)) (frame-statistics v))
    (let ((hr (logand (m (view-swap v) 8 (if *flip* 1 0) 0) #xFFFFFFFF)))   ; Present: on the refresh (flip)
      (unless (or (zerop hr) (eql hr *last-present*))
        (format t "~&vessel-windows: Present answered #x~8,'0X~%" hr))
      (setf *last-present* hr))
    0))

(defvar *last-present* 0)

(defun frame-statistics (v)
  "DXGI_FRAME_STATISTICS (GetFrameStatistics, slot 16): PresentCount at +0, PresentRefreshCount at +4.  Between two
   readings, every refresh more than there were presents showed a frame a second time."
  (let ((fs (area 3)))
    (zero fs 32)
    (when (zerop (logand (m (view-swap v) 16 fs) #xFFFFFFFF))
      (let ((pc (peek32 fs)) (rc (peek32 (+ fs 4))))
        (when *stat-dxgi*
          (destructuring-bind (pc0 rc0) *stat-dxgi*
            (let ((dp (- pc pc0)) (dr (- rc rc0)))
              (when (and (plusp pc0) (plusp dp) (> dr dp) (< dr 1000)) (incf *stat-repeats* (- dr dp))))))
        (unless *stat-dxgi* (format t "~&vessel-windows: DXGI frame statistics: present ~D, refresh ~D~%" pc rc))
        (setf *stat-dxgi* (list pc rc))))))

(defun view-size (h)
  (let ((v (view-of h)))
    (if (zerop (view-hwnd v))
        0
        (multiple-value-bind (w hh) (hwnd-size (view-hwnd v)) (+ (* w 65536) hh)))))

(defun topology (prim) (aref #(4 5 2 1) prim))

(defun buffer-new (addr n)
  (let* ((size (max 16 (* 16 (ceiling n 16))))
         (d (area 0)))
    (zero d 24)
    (poke32 d size) (poke32 (+ d 8) #xB) (poke32 (+ d 16) #x20)                ; VERTEX|INDEX|SHADER_RESOURCE, RAW
    (let ((buf (created (m *dev* 3 d 0 (out0)) "CreateBuffer")))
      (when (and (/= addr 0) (plusp n)) (buffer-write buf 0 addr n))   ; ADDR: a token (negative) or an address
      (let ((v (area 1)))
        (zero v 24)
        (poke32 v 39) (poke32 (+ v 4) 11) (poke32 (+ v 12) (floor size 4)) (poke32 (+ v 16) 1)   ; R32_TYPELESS BUFFEREX RAW
        (gpu-handle :buffer buf (created (m *dev* 7 buf v (out0)) "CreateShaderResourceView (buffer)") size)))))

(defun buffer-write (buf offset addr n)
  (let ((box (area 2)))
    (poke32 box offset) (poke32 (+ box 4) 0) (poke32 (+ box 8) 0)
    (poke32 (+ box 12) (+ offset n)) (poke32 (+ box 16) 1) (poke32 (+ box 20) 1)
    (m *ctx* 48 buf 0 box (native-of addr n) 0 0)
    0))

(defun texture-new (addr w h)
  (let ((d (area 0)))
    (zero d 44)
    (poke32 d w) (poke32 (+ d 4) h) (poke32 (+ d 8) 0) (poke32 (+ d 12) 1) (poke32 (+ d 16) 28)
    (poke32 (+ d 20) 1) (poke32 (+ d 32) #x28) (poke32 (+ d 40) 1)               ; SRV|RT, GENERATE_MIPS
    (let ((tex (created (m *dev* 5 d 0 (out0)) "CreateTexture2D")))
      (m *ctx* 48 tex 0 0 (native-of addr (* 4 w h)) (* 4 w) 0)
      (let ((srv (created (m *dev* 7 tex 0 (out0)) "CreateShaderResourceView (texture)")))
        (m *ctx* 54 srv)
        (gpu-handle :texture tex srv)))))

(defun blob-text (blob)
  (if (zerop blob) "" (modus-int::w32-astr (m blob 3) (logand (m blob 4) #xFFFFFF))))

(defun compile-hlsl (src n entry target)
  (poke64 (cell 0) 0) (poke64 (cell 1) 0)
  (let ((hr (c *d3d-compile* src n 0 0 0 (ascii1 entry) (ascii2 target) 0 0 (cell 0) (cell 1))))
    (let ((code (peek64 (cell 0))) (errors (peek64 (cell 1))))
      (when (failed-p hr)
        (format t "~&vessel-windows: ~A (~A) did not compile:~%~A~%" entry target (blob-text errors))
        (release errors) (release code)
        (return-from compile-hlsl 0))
      (release errors)
      code)))

(defun pipeline-new (shader vaddr vn faddr fn)
  (destructuring-bind (src n) (gpu-data shader)
    (let ((vb (compile-hlsl src n (utf8-text vaddr vn) "vs_5_0"))
          (pb (compile-hlsl src n (utf8-text faddr fn) "ps_5_0")))
      (if (or (zerop vb) (zerop pb))
          (progn (release vb) (release pb) 0)
          (let ((vs (created (m *dev* 12 (m vb 3) (m vb 4) 0 (out0)) "CreateVertexShader"))
                (ps (created (m *dev* 15 (m pb 3) (m pb 4) 0 (out0)) "CreatePixelShader")))
            (release vb) (release pb)
            (gpu-handle :pipeline vs ps))))))

(defun shader-new (addr n)
  (let ((copy (modus-int::w32-alloc (+ n 1))))
    (c *memcpy* copy (native-of addr n) n)
    (gpu-handle :shader copy n)))

(defun set-bytes (addr n index stage)
  (let ((b (cbuf index)) (mapped (area 3)))
    (zero mapped 16)
    (check (m *ctx* 14 b 0 4 0 mapped) "Map")    ; WRITE_DISCARD
    (c *memcpy* (peek64 mapped) (native-of addr (min n 4096)) (min n 4096))
    (m *ctx* 15 b 0)
    (unless (= stage 1) (m *ctx* 7 index 1 (ptr-cell 6 b)))
    (unless (= stage 0) (m *ctx* 16 index 1 (ptr-cell 6 b)))
    0))

;;; THE FIRST VERTEX.  Metal's vertex_id counts from the draw's first vertex (a draw of 6 from 300 sees 300..305);
;;; Direct3D's SV_VertexID counts from 0 whatever StartVertexLocation says, and leaves out an indexed draw's base
;;; vertex.  So each draw's first vertex is in the cbuffer b13 (its first uint), bound to the vertex stage, and a
;;; shader adds it to SV_VertexID: then one buffer of several shapes, drawn a shape at a time, reads as in Metal.

(defconstant +draw-slot+ 13)

(defun first-vertex (v)
  (unless (eql v *first-vertex*)
    (setf *first-vertex* v)
    (let ((b (cbuf +draw-slot+)) (mapped (area 3)))
      (zero mapped 16)
      (check (m *ctx* 14 b 0 4 0 mapped) "Map (first vertex)")
      (poke32 (peek64 mapped) v)
      (m *ctx* 15 b 0)
      (m *ctx* 7 +draw-slot+ 1 (ptr-cell 6 b)))))

;;; ---- more windows ----------------------------------------------------------------------------------------------------
;;;
;;; window_new: a window of its own (hello3d's Map), with its own window procedure page (the main window's page knows
;;; only the main window), its own class, and a view added to it (ui_add) drawn in it.  A palette is a tool window owned
;;; by the app's window: over it, never taking the keyboard.  Its close button hides it (:winclose); its size is
;;; reported (:winresize, points).

(defun window-new (addr n w h palette)
  (let* ((page (make-page))
         (class (format nil "VesselWindow~D" (length *pages*)))
         (style (if (/= palette 0) #x00CC0000 +main-style+))         ; a palette: caption, close, resizable
         (ex (if (/= palette 0) #x08000080 0)))                      ; WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE
    (dolist (off '(#x40 #x48 #x50 #x58)) (poke64 (+ page off) (peek64 (+ *page* off))))
    (poke64 (+ page #x40) *arrow*)
    (register-class class *arrow* 0 page)
    (let ((r (rect-area)))
      (zero r 16)
      (poke32 (+ r 8) (px w)) (poke32 (+ r 12) (px h))
      (c *adjust-rect* r style 0 ex)
      (let ((hwnd (u *create-window* ex (wide1 class) (wide2 (utf8-text addr n)) style #x80000000 #x80000000
                     (- (s32 (peek32 (+ r 8))) (s32 (peek32 r))) (- (s32 (peek32 (+ r 12))) (s32 (peek32 (+ r 4))))
                     (if (/= palette 0) *hwnd* 0) 0 *hinst* 0)))
        (when (zerop hwnd) (error "vessel-windows: no window ~A" class))
        (poke64 (+ page #x10) hwnd)
        (setf *pages* (append *pages* (list page)))
        (u *show-window* hwnd (if (/= palette 0) 4 5))              ; SW_SHOWNOACTIVATE for a palette
        (new-handle (make-ctl :kind :window :hwnd hwnd :data (list page (/= palette 0))))))))

(defun window-message (h msg wp lp)
  "A message for the window H (made by window_new)."
  (case msg
    (#x10 (u *show-window* (ctl-hwnd (ctl-of h)) 0) (ui-event (list :winclose h)))
    (#x05 (unless (= wp 1)
            (ui-event (list :winresize h (+ (* 65536 (round (* 96 (lo16 lp)) *dpi*)) (round (* 96 (hi16 lp)) *dpi*))))))))

;;; ---- game controllers: XInput --------------------------------------------------------------------------------------

(defvar *rumble-until* (make-array 4 :initial-element nil))

(defvar *pad-absent* (make-array 4 :initial-element nil)
  "For each slot found empty: when it may be asked again (internal time).")

(defun xinput-state (index)
  "Controller INDEX's XINPUT_GAMEPAD at (area 3), or NIL.  An EMPTY slot is asked at most once a second: XInput
   looks for the device each time it is asked about one that is not there, which takes milliseconds -- and a game
   asks every frame."
  (and (plusp *xinput-get*) (< -1 index 4)
       (let ((again (aref *pad-absent* index)))
         (or (null again) (>= (get-internal-real-time) again)))
       (let ((s (area 3)))
         (zero s 16)
         (if (zerop (logand (c *xinput-get* index s) #xFFFFFFFF))
             (progn (setf (aref *pad-absent* index) nil) (+ s 4))
             (progn (setf (aref *pad-absent* index) (+ (get-internal-real-time) internal-time-units-per-second))
                    nil)))))

(defun pad-count () (loop for i below 4 count (xinput-state i)))

(defun pad-state (index addr n)
  (let ((g (xinput-state index)))
    (if (null g)
        0
        (let* ((b (peek16 g))
               (axis (lambda (v) (max -1.0 (/ (s16 v) 32767.0))))
               (vals (list (funcall axis (peek16 (+ g 4))) (funcall axis (peek16 (+ g 6)))
                           (funcall axis (peek16 (+ g 8))) (funcall axis (peek16 (+ g 10)))
                           (/ (peek8 (+ g 2)) 255.0) (/ (peek8 (+ g 3)) 255.0)
                           (- (if (logtest b 8) 1.0 0.0) (if (logtest b 4) 1.0 0.0))       ; d-pad x
                           (- (if (logtest b 1) 1.0 0.0) (if (logtest b 2) 1.0 0.0)))))     ; d-pad y
          (dolist (bit '(#x1000 #x2000 #x4000 #x8000 #x100 #x200 #x40 #x80 #x10 #x20 #x400))
            (setf vals (nconc vals (list (if (logtest b bit) 1.0 0.0)))))
          (put-floats addr (subseq vals 0 (min n 19)))
          1))))

(defun pad-rumble (index strength ms)
  (when (and (plusp *xinput-set*) (< -1 index 4))
    (let ((v (area 3)) (s (min 65535 (round (* 65535 strength) 1000))))
      (poke16 v s) (poke16 (+ v 2) s)
      (c *xinput-set* index v)
      (after (/ ms 1000.0) (lambda () (let ((v (area 3))) (poke32 v 0) (c *xinput-set* index v))))))
  0)

(defun put-utf8 (text addr cap)
  "TEXT's UTF-8 at ADDR, at most CAP bytes; how many."
  (let ((n (cl-user::%utf8-length text)))
    (if (> n cap)
        0
        (let ((b (make-array n :element-type '(unsigned-byte 8))))
          (cl-user::%utf8-put text b)
          (put-bytes addr b)
          n))))

;;; ---- where this PC is: Windows.Devices.Geolocation ------------------------------------------------------------------
;;;
;;; The Geolocator, through modus's WinRT layer (net/win32-winrt.lisp).  RequestAccessAsync answers whether this app
;;; may know (Settings > Privacy & security > Location: location services, and "Let desktop apps access your
;;; location"); a fix is GetGeopositionAsync's Geoposition -> Coordinate -> latitude, longitude, accuracy.  Nothing
;;; waits: each operation is looked at a frame at a time (timers) until it ends.
;;; IGeolocatorStatics 6 RequestAccessAsync.  IGeolocator 13 GetGeopositionAsync.  IGeoposition 6 get_Coordinate.
;;; IGeocoordinate 6 get_Latitude, 7 get_Longitude, 9 get_Accuracy (doubles).

(defvar *wrt* nil)
(defvar *location-permission* 0 "As location_permission answers: 0 not asked, 1 allowed, 2 refused.")
(defvar *fix* nil "(latitude longitude accuracy altitude), once there is one.")
(defvar *locating* nil)

(defparameter *geolocator* "Windows.Devices.Geolocation.Geolocator")

(defun wrt ()
  (or *wrt* (setf *wrt* (or (modus-int::wrt-open) (error "vessel-windows: no Windows Runtime")))))

(defun when-done (op fn)
  "Look at the WinRT operation OP a frame at a time; when it ends, FN with T and its result, or NIL and 0."
  (labels ((look ()
             (when *debug* (format t "~&vessel-windows: WinRT operation ~X ~(~A~)~%" op (modus-int::wrt-op-state (wrt) op)))
             (case (modus-int::wrt-op-state (wrt) op)
               (:running (after 0.05 #'look))
               (:done (funcall fn t (or (modus-int::wrt-op-result (wrt) op) 0)))
               (t (modus-int::wrt-release (wrt) op) (funcall fn nil 0)))))
    (look)))

(defun location-ask ()
  (let* ((ctx (wrt))
         (statics (modus-int::wrt-factory ctx *geolocator* "9A8E7571-2DF5-4591-9F87-EB5FD894E9B7")))
    (flet ((answer (ok)
             (setf *location-permission* (if ok 1 2))
             (format t "~&vessel-windows: location ~:[refused~;allowed~]~%" ok)
             (ui-event (list :locationask 0 (if ok 1 0)))))
      (if (zerop statics)
          (answer nil)
          (let ((hr (modus-int::wrt-call ctx statics 6 (modus-int::wrt-out-clear ctx 0))))
            (let ((op (modus-int::wrt-out-ref ctx 0)))
              (modus-int::wrt-release ctx statics)
              (if (or (< hr 0) (zerop op))
                  (answer nil)
                  (when-done op (lambda (done status) (answer (and done (= (logand status #xFFFFFFFF) 1))))))))))))

(defun location-start ()
  (unless *locating*
    (setf *locating* t)
    (let* ((ctx (wrt))
           (inspectable (modus-int::wrt-activate ctx *geolocator*))
           (geo (modus-int::wrt-qi ctx inspectable "A9C3BF62-4524-4989-8AA9-DE019D2E551F")))
      (modus-int::wrt-release ctx inspectable)
      (if (zerop geo)
          (progn (setf *locating* nil) (format t "~&vessel-windows: no Geolocator~%"))
          (let ((hr (modus-int::wrt-call ctx geo 13 (modus-int::wrt-out-clear ctx 0))))
            (let ((op (modus-int::wrt-out-ref ctx 0)))
              (modus-int::wrt-release ctx geo)
              (if (or (< hr 0) (zerop op))
                  (setf *locating* nil)
                  (when-done op
                    (lambda (done pos)
                      (setf *locating* nil)
                      (if (or (not done) (zerop pos))
                          (format t "~&vessel-windows: no position from Windows~%")
                          (let* ((coord (progn (modus-int::wrt-call ctx pos 6 (modus-int::wrt-out-clear ctx 1))
                                               (modus-int::wrt-out-ref ctx 1)))
                                 (get (lambda (slot)
                                        (modus-int::wrt-call ctx coord slot (modus-int::wrt-out-clear ctx 2))
                                        (modus-int::%w32-bits-double (modus-int::wrt-out-ref ctx 2)))))
                            (unless (zerop coord)
                              (setf *fix* (list (funcall get 6) (funcall get 7) (funcall get 9) 0.0))
                              (modus-int::wrt-release ctx coord))
                            (modus-int::wrt-release ctx pos))))))))))))

;;; ---- the speaker -----------------------------------------------------------------------------------------------------
;;;
;;; THE APP KEEPS A RING FED, A NATIVE THREAD KEEPS THE DEVICE FED -- modus's host/macos/modus-audio.c, on WASAPI.
;;; 1011 only copies samples into a ring in native memory; a thread of native code written here (no Lisp) waits on the
;;; device's event, asks how much room it has (GetCurrentPadding), and copies that much from the ring (GetBuffer /
;;; ReleaseBuffer).  Nothing on the app's thread calls the audio system, so a slow frame or a collection costs the
;;; speaker nothing while the ring holds out, and the device's own clock sets the pace.  Shared mode, 16-bit 48 kHz
;;; stereo as sound.lisp mixes it; Windows converts to the device's own format (AUTOCONVERTPCM).  If WASAPI will not
;;; start, waveOut (net/win32-audio.lisp) as before.
;;;
;;; The block: +#x00 WaitForSingleObject  +#x08 memcpy  +#x10 the event  +#x18 IAudioClient  +#x20 IAudioRenderClient
;;; +#x28 the device buffer's frames  +#x30 the ring  +#x38 its size - 1  +#x40 bytes ever written (dword, the app's)
;;; +#x48 bytes ever read (dword, the thread's)  +#x50 the device's padding (frames)  +#x58 underruns: wakes that found
;;; the device empty once sound had begun -- it had played silence, whatever the ring held  +#x68 a padding cell  +#x70 a buffer pointer cell
;;; +#x60 the lowest padding since the app last looked  +#x78 frames being copied  +#x80 stop  +#x100 the code.  The two counters are dwords, so each side reads and
;;; writes them whole, and wrap with the ring (its size divides 2^32).
;;; IAudioClient 3 Initialize, 4 GetBufferSize, 6 GetCurrentPadding, 10 Start, 13 SetEventHandle, 14 GetService;
;;; IAudioRenderClient 3 GetBuffer, 4 ReleaseBuffer; IMMDeviceEnumerator 4 GetDefaultAudioEndpoint; IMMDevice 3 Activate.

(defvar *speaker* nil "The speaker: :WASAPI, or W32-AUDIO-OPEN's waveOut speaker, once opened.")
(defvar *wa* 0 "The WASAPI block.")
(defvar *audio-clock* 0 "IAudioClock: how far the device has really played.")
(defvar *audio-clock-hz* 0)
(defvar *clock-last* nil "The last (device-position qpc-position), for DEVICE-DRIFT.")

(defun device-drift ()
  "Milliseconds the device's playback fell behind (or ran ahead of) the system clock since the last call: a stall or
   a skip somewhere after the ring -- the engine, the driver, the device.  NIL before there are two readings."
  (unless (zerop *audio-clock*)
    (when (zerop (logand (m *audio-clock* 4 (cell 2) (cell 3)) #xFFFFFFFF))   ; GetPosition(&pos, &qpc 100ns)
      (let ((pos (peek64 (cell 2))) (qpc (peek64 (cell 3))) (out nil))
        (when (and *clock-last* (plusp *audio-clock-hz*))
          (destructuring-bind (p0 q0) *clock-last*
            (let ((played-ms (/ (* 1000.0 (- pos p0)) *audio-clock-hz*))
                  (wall-ms (/ (- qpc q0) 10000.0)))
              (setf out (- played-ms wall-ms)))))
        (setf *clock-last* (list pos qpc))
        out))))
(defconstant +ring-bytes+ (ash 1 20))
(defvar *pcm* nil)

(defun wasapi-code ()
  (let ((jne '(#x0F #x85)) (je '(#x0F #x84)) (js '(#x0F #x88)) (jle '(#x0F #x8E)) (jbe '(#x0F #x86)) (jmp '(#xE9))
        (jae '(#x0F #x83)))
    (assemble
     `(#x53 #x56 #x57 #x48 #x83 #xEC #x30          ; push rbx/rsi/rdi; sub rsp,0x30
       #x48 #x89 #xCB                              ; mov rbx,rcx    the block
       (:label :loop)
       #x48 #x83 #xBB #x80 #x00 #x00 #x00 #x00 (:j ,jne :out)    ; stop?
       #x48 #x8B #x4B #x10 #xBA #x64 #x00 #x00 #x00 #xFF #x13    ; WaitForSingleObject(event, 100)
       #x48 #x8B #x4B #x18 #x48 #x8D #x53 #x68     ; rcx client, rdx &padding
       #x48 #x8B #x01 #xFF #x50 #x30               ; GetCurrentPadding
       #x85 #xC0 (:j ,js :loop)
       #x8B #x43 #x68 #x48 #x89 #x43 #x50          ; padding -> +0x50
       #x48 #x3B #x43 #x60 (:j ,jae :not-lower)    ; the lowest this second -> +0x60
       #x48 #x89 #x43 #x60
       (:label :not-lower)
       #x48 #x85 #xC0 (:j ,jne :nonempty)          ; the device empty once sound has begun: it played silence
       #x83 #x7B #x40 #x00 (:j ,je :nonempty)
       #x48 #xFF #x43 #x58                         ; an underrun
       #x8B #x43 #x68                              ; eax = padding again (0)
       (:label :nonempty)
       #x48 #x8B #x73 #x28 #x48 #x29 #xC6 (:j ,jle :loop)        ; rsi = room (frames)
       #x8B #x7B #x40 #x2B #x7B #x48 #xC1 #xEF #x02              ; edi = frames in the ring
       #x85 #xFF (:j ,je :loop)                    ; nothing in the ring
       (:label :have)
       #x48 #x39 #xFE (:j ,jbe :keep) #x48 #x89 #xFE   ; rsi = min(room, ring)
       (:label :keep)
       #x48 #x89 #x73 #x78                         ; frames -> +0x78
       #x48 #x8B #x4B #x20 #x89 #xF2 #x4C #x8D #x43 #x70   ; rcx render, edx frames, r8 &data
       #x48 #x8B #x01 #xFF #x50 #x18               ; GetBuffer
       #x85 #xC0 (:j ,js :loop)
       #x48 #x8B #x73 #x78 #x48 #xC1 #xE6 #x02     ; rsi = bytes
       #x8B #x43 #x48 #x48 #x23 #x43 #x38          ; rax = read offset in the ring
       #x48 #x8B #x7B #x38 #x48 #xFF #xC7 #x48 #x29 #xC7   ; rdi = ring size - offset
       #x48 #x39 #xF7 (:j ,jbe :first) #x48 #x89 #xF7      ; rdi = min(that, bytes)
       (:label :first)
       #x48 #x8B #x4B #x70 #x48 #x8B #x53 #x30 #x48 #x01 #xC2 #x49 #x89 #xF8 #xFF #x53 #x08   ; memcpy(data, ring+off, rdi)
       #x49 #x89 #xF0 #x49 #x29 #xF8 (:j ,je :copied)      ; the rest, from the ring's start
       #x48 #x8B #x4B #x70 #x48 #x01 #xF9 #x48 #x8B #x53 #x30 #xFF #x53 #x08
       (:label :copied)
       #x01 #x73 #x48                              ; read += bytes (dword)
       #x48 #x8B #x4B #x20 #x8B #x53 #x78 #x45 #x31 #xC0   ; rcx render, edx frames, r8 0
       #x48 #x8B #x01 #xFF #x50 #x20               ; ReleaseBuffer
       (:j ,jmp :loop)
       (:label :out)
       #x48 #x83 #xC4 #x30 #x5F #x5E #x5B #x31 #xC0 #xC3))))

(defun wasapi-open (rate channels)
  "WASAPI on the default output, its feeding thread started; T, or NIL (and why) if it cannot be."
  (c *co-initialize* 0 0)                                            ; this thread in the multithreaded apartment
  (flet ((fail (why) (format t "~&vessel-windows: WASAPI: ~A; waveOut instead~%" why) (return-from wasapi-open nil))
         (ok (hr) (not (failed-p hr))))
    (let ((g1 (area 1)) (g2 (area 2)) (enum 0) (dev 0) (client 0) (render 0))
      (modus-int::w32-put-guid g1 "BCDE0395-E52F-467C-8E3D-C4579291692E")
      (modus-int::w32-put-guid g2 "A95664D2-9614-4F35-A746-DE8DB63617E6")
      (unless (ok (c *co-create* g1 0 23 g2 (out0))) (fail "no device enumerator"))
      (setf enum (peek64 (cell 0)))
      (unless (ok (m enum 4 0 0 (out0))) (release enum) (fail "no default output"))    ; eRender, eConsole
      (setf dev (peek64 (cell 0)))
      (release enum)
      (modus-int::w32-put-guid g1 "1CB9AD4C-DBFA-4C32-B178-C2F568A703B2")
      (unless (ok (m dev 3 g1 23 0 (out0))) (release dev) (fail "no audio client"))
      (setf client (peek64 (cell 0)))
      (release dev)
      (let ((wf (area 0)))                                               ; WAVEFORMATEX: PCM, 16-bit
        (zero wf 20)
        (poke16 wf 1) (poke16 (+ wf 2) channels) (poke32 (+ wf 4) rate) (poke32 (+ wf 8) (* rate channels 2))
        (poke16 (+ wf 12) (* channels 2)) (poke16 (+ wf 14) 16)
        ;; shared; EVENTCALLBACK | AUTOCONVERTPCM | SRC_DEFAULT_QUALITY; a 60 ms device buffer: slack for a late wake
        (unless (ok (m client 3 0 #x88040000 600000 0 wf 0)) (release client) (fail "Initialize refused")))
      ;; the device's own mix format: what Windows converts our 16-bit 48 kHz stereo to
      (when (not (failed-p (m client 8 (out0))))                         ; GetMixFormat
        (let ((mf (peek64 (cell 0))))
          (format t "~&vessel-windows: the device mixes ~D Hz, ~D channel~:P, ~D bits~:[~; (resampled from 48000)~]~%"
                  (peek32 (+ mf 4)) (peek16 (+ mf 2)) (peek16 (+ mf 14)) (/= (peek32 (+ mf 4)) rate))))
      (let ((event (c *create-event* 0 0 0 0)))
        (m client 13 event)                                              ; SetEventHandle
        (poke32 (cell 1) 0)
        (m client 4 (cell 1))                                            ; GetBufferSize
        (modus-int::w32-put-guid g1 "F294ACFC-3146-4483-A7BF-ADDCA7C260E2")
        (unless (ok (m client 14 g1 (out0))) (release client) (fail "no render client"))
        (setf render (peek64 (cell 0)))
        (modus-int::w32-put-guid g1 "CD63314F-3FBA-4A1B-812C-EF96358728E7")       ; IAudioClock: the device's position
        (when (ok (m client 14 g1 (out0)))
          (setf *audio-clock* (peek64 (cell 0)))
          (m *audio-clock* 3 (cell 4))                                           ; GetFrequency (not cell 1: the buffer size)
          (setf *audio-clock-hz* (peek64 (cell 4))))
        (let ((block (modus-int::w32-alloc #x1000 t)) (ring (modus-int::w32-alloc +ring-bytes+)))
          (poke64 block *wait-single*) (poke64 (+ block 8) *memcpy*) (poke64 (+ block #x10) event)
          (poke64 (+ block #x18) client) (poke64 (+ block #x20) render) (poke64 (+ block #x28) (peek32 (cell 1)))
          (poke64 (+ block #x30) ring) (poke64 (+ block #x38) (- +ring-bytes+ 1))
          (loop for b in (wasapi-code) for i from 0 do (poke8 (+ block #x100 i) b))
          (poke64 (+ block #x60) #xFFFFFFFF)
          (setf *wa* block)
          (let ((thread (c *create-thread* 0 0 (+ block #x100) block 0 0)))
            (c *set-thread-priority* thread 2))                          ; THREAD_PRIORITY_HIGHEST
          (unless (ok (m client 10)) (fail "Start refused"))             ; Start
          (format t "~&vessel-windows: speaker: WASAPI, a ~D-frame device buffer, fed by its own thread~%"
                  (peek32 (cell 1)))
          (when *loopback* (ignore-errors (loopback-open)))
          t)))))

(defun audio-open (rate channels)
  (unless *speaker*
    (setf *speaker* (or (and (not (equal (modus-int::%cli-getenv "VESSEL_WAVEOUT") "1"))
                             (ignore-errors (wasapi-open rate (max 1 channels)))
                             :wasapi)
                        (modus-int::w32-audio-open rate (max 1 channels))))
    (when (and *speaker* (not (eq *speaker* :wasapi)) (not (= *wa* 0))) nil))
  (if *speaker* 0 -1))

(defun wasapi-p () (eq *speaker* :wasapi))

(defun ring-filled () (logand (- (peek32 (+ *wa* #x40)) (peek32 (+ *wa* #x48))) #xFFFFFFFF))

;;; WHAT WINDOWS ACTUALLY PLAYS (VESSEL_LOOPBACK=PATH): the output device's final mix -- every app's sound, after the
;;; audio engine -- captured in loopback mode for 30 s and written to PATH as a float WAV, with each packet the engine
;;; marked DATA_DISCONTINUITY (a glitch of its own) and each change of focus logged against the recording's clock.  A
;;; hole there is in Windows' mix; a clean recording while a pop is heard puts the pop after Windows (HDMI, the TV).
;;; IAudioCaptureClient 3 GetBuffer, 4 ReleaseBuffer, 5 GetNextPacketSize.

(defvar *loopback* (let ((p (modus-int::%cli-getenv "VESSEL_LOOPBACK"))) (and p (plusp (length p)) p)))
(defvar *lb-capture* 0)
(defvar *lb-buf* 0)
(defvar *lb-frame-bytes* 8)
(defvar *lb-frames* 0)
(defvar *lb-max* (* 48000 30))
(defvar *lb-glitches* '() "Milliseconds into the recording where the engine flagged a discontinuity.")
(defvar *lb-marks* '() "(milliseconds what): the focus changes, against the recording.")
(defvar *lb-done* nil)

(defun lb-ms () (round (* 1000 *lb-frames*) 48000))

(defun lb-mark (what) (when (and *loopback* (plusp *lb-capture*) (not *lb-done*)) (push (list (lb-ms) what) *lb-marks*)))

(defun loopback-open ()
  (let ((g1 (area 1)) (g2 (area 2)))
    (modus-int::w32-put-guid g1 "BCDE0395-E52F-467C-8E3D-C4579291692E")
    (modus-int::w32-put-guid g2 "A95664D2-9614-4F35-A746-DE8DB63617E6")
    (when (failed-p (c *co-create* g1 0 23 g2 (out0))) (return-from loopback-open nil))
    (let ((enum (peek64 (cell 0))))
      (when (failed-p (m enum 4 0 0 (out0))) (release enum) (return-from loopback-open nil))
      (let ((dev (peek64 (cell 0))))
        (release enum)
        (modus-int::w32-put-guid g1 "1CB9AD4C-DBFA-4C32-B178-C2F568A703B2")
        (when (failed-p (m dev 3 g1 23 0 (out0))) (release dev) (return-from loopback-open nil))
        (let ((client (peek64 (cell 0))))
          (release dev)
          (m client 8 (out0))                                              ; GetMixFormat
          (let ((mf (peek64 (cell 0))))
            (setf *lb-frame-bytes* (peek16 (+ mf 12)))
            ;; shared, LOOPBACK, a 1 s buffer, the device's own format
            (when (failed-p (m client 3 0 #x20000 10000000 0 mf 0))
              (format t "~&vessel-windows: loopback: Initialize refused~%") (return-from loopback-open nil)))
          (modus-int::w32-put-guid g1 "C8ADBD64-E71E-48A0-A4DE-185C395CD317")
          (when (failed-p (m client 14 g1 (out0))) (return-from loopback-open nil))
          (setf *lb-capture* (peek64 (cell 0))
                *lb-buf* (modus-int::w32-alloc (* *lb-max* *lb-frame-bytes*)))
          (m client 10)                                                    ; Start
          (format t "~&vessel-windows: recording what Windows plays: 30 s to ~A~%" *loopback*)
          t)))))

(defun loopback-poll ()
  (when (and (plusp *lb-capture*) (not *lb-done*))
    (loop
      (poke32 (cell 10) 0)
      (when (failed-p (m *lb-capture* 5 (cell 10))) (return))             ; GetNextPacketSize
      (when (zerop (peek32 (cell 10))) (return))
      (poke64 (cell 11) 0) (poke32 (cell 12) 0) (poke32 (cell 13) 0)
      (when (failed-p (m *lb-capture* 3 (cell 11) (cell 12) (cell 13) 0 0)) (return))   ; GetBuffer
      (let ((frames (peek32 (cell 12))) (flags (peek32 (cell 13))) (data (peek64 (cell 11))))
        (when (logtest flags 1) (push (lb-ms) *lb-glitches*))              ; DATA_DISCONTINUITY
        (let ((n (min frames (- *lb-max* *lb-frames*))))
          (when (plusp n)
            (let ((dst (+ *lb-buf* (* *lb-frames* *lb-frame-bytes*))) (bytes (* n *lb-frame-bytes*)))
              (if (logtest flags 2)                                        ; SILENT
                  (zero dst bytes)
                  (c *memcpy* dst data bytes)))
            (incf *lb-frames* n)))
        (m *lb-capture* 4 frames))                                         ; ReleaseBuffer
      (when (>= *lb-frames* *lb-max*) (loopback-write) (return)))))

(defun loopback-write ()
  (setf *lb-done* t)
  (let* ((data (* *lb-frames* *lb-frame-bytes*)) (hdr (modus-int::w32-alloc 64)))
    (flet ((put32 (o v) (poke32 (+ hdr o) v)) (put16 (o v) (poke16 (+ hdr o) v))
           (tag (o text) (loop for ch across text for k from 0 do (poke8 (+ hdr o k) (char-code ch)))))
      (tag 0 "RIFF") (put32 4 (+ 36 data)) (tag 8 "WAVEfmt ") (put32 16 16) (put16 20 3) (put16 22 2)
      (put32 24 48000) (put32 28 (* 48000 *lb-frame-bytes*)) (put16 32 *lb-frame-bytes*) (put16 34 32)
      (tag 36 "data") (put32 40 data))
    ;; CreateFileW / WriteFile: 11 MB from native memory, no Lisp vector in between
    (let ((h (w32-call-file *loopback*)))
      (when h
        (w32-write-all h hdr 44)
        (w32-write-all h *lb-buf* data)
        (c *close-handle* h)))
    (format t "~&vessel-windows: what Windows played, ~D ms, written to ~A~%" (lb-ms) *loopback*)
    (format t "~&vessel-windows: the engine's own glitches (discontinuities) at ms: ~S~%" (reverse *lb-glitches*))
    (format t "~&vessel-windows: focus changes at ms: ~S~%" (reverse *lb-marks*))
    (finish-output)))

(defun w32-call-file (path)
  (let ((h (c *create-file* (wide1 path) #x40000000 0 0 2 #x80 0)))       ; GENERIC_WRITE, CREATE_ALWAYS
    (if (or (zerop h) (= h #xFFFFFFFFFFFFFFFF)) nil h)))

(defun w32-write-all (h addr n)
  (loop while (plusp n)
        do (let ((chunk (min n #x100000)))
             (poke32 (cell 14) 0)
             (c *write-file* h addr chunk (cell 14) 0)
             (let ((done (peek32 (cell 14))))
               (when (zerop done) (return))
               (incf addr done) (decf n done)))))

;;; GAPS, COUNTED (1013, as the Mac's AUDIO-UNDERRUNS).  WASAPI: the feeding thread counts each time the device was
;;; empty with nothing in the ring.  waveOut: a write that finds nothing queued, once sound has begun.  A write that
;;; finds no room is dropped.  Both are counted, and a line logged each second either grows.

(defvar *audio-started* nil)
(defvar *audio-gaps* 0 "waveOut: writes that found the speaker dry.")
(defvar *audio-dropped* 0 "Writes dropped: no room.")
(defvar *audio-said* (list 0 0))
(defvar *audio-least* nil "The fewest frames found queued at a write this second.")
(defvar *audio-seams* 0 "Chunk boundaries this second where a channel jumped more than 1/16 of full scale: a click.")
(defvar *audio-last* (list 0 0) "The last chunk's last left and right samples.")

(defvar *record* (let ((p (modus-int::%cli-getenv "VESSEL_RECORD"))) (and p (plusp (length p)) p))
  "VESSEL_RECORD=PATH: the first 20 seconds of what the app hands the speaker, written to PATH as a WAV.")
(defvar *recording* nil)
(defvar *recording-at* 0)

(defun record-samples (src n)
  (when (and *record* (not (eq *recording* :done)))
    (unless *recording* (setf *recording* (make-array (* 2 48000 20) :element-type '(signed-byte 16) :initial-element 0)))
    (dotimes (i n)
      (when (< *recording-at* (length *recording*))
        (setf (aref *recording* *recording-at*) (ash (s32 (peek32 (+ src (* 8 i)))) -1))
        (incf *recording-at*)))
    (when (>= *recording-at* (length *recording*))
      (let* ((data (* 2 (length *recording*))) (b (make-array (+ 44 data) :element-type '(unsigned-byte 8))))
        (flet ((put32 (o v) (dotimes (k 4) (setf (aref b (+ o k)) (ldb (byte 8 (* 8 k)) v))))
               (put16 (o v) (dotimes (k 2) (setf (aref b (+ o k)) (ldb (byte 8 (* 8 k)) v)))))
          (loop for ch across "RIFF" for k from 0 do (setf (aref b k) (char-code ch)))
          (put32 4 (+ 36 data))
          (loop for ch across "WAVEfmt " for k from 8 do (setf (aref b k) (char-code ch)))
          (put32 16 16) (put16 20 1) (put16 22 2) (put32 24 48000) (put32 28 192000) (put16 32 4) (put16 34 16)
          (loop for ch across "data" for k from 36 do (setf (aref b k) (char-code ch)))
          (put32 40 data)
          (loop for v across *recording* for k from 44 by 2 do (put16 k (logand v #xFFFF))))
        (with-open-file (f *record* :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence b f))
        (format t "~&vessel-windows: 20 s of the speaker's samples written to ~A~%" *record*)
        (setf *recording* :done)))))

(defun audio-gap-count () (if (wasapi-p) (peek64 (+ *wa* #x58)) *audio-gaps*))

(defun audio-write (src n)
  "N samples from SRC, a simple-vector of fixnums as modus stores them (value<<1 a word)."
  (cond
    ((null *speaker*) -1)
    ((wasapi-p)
     (record-samples src n)
     (let* ((wr (peek32 (+ *wa* #x40))) (ring (peek64 (+ *wa* #x30))) (mask (- +ring-bytes+ 1))
            (q (floor (+ (ring-filled) (* 4 (peek64 (+ *wa* #x50)))) 4)))
       (setf *audio-least* (if *audio-least* (min *audio-least* q) q))
       (if (> (+ (ring-filled) (* 2 n)) +ring-bytes+)
           (progn (incf *audio-dropped*) 0)
           (let ((l0 0) (r0 0) (l 0) (r 0))
             ;; a stereo frame a dword: left low, right high
             (loop for i from 0 below n by 2
                   do (setf l (ash (s32 (peek32 (+ src (* 8 i)))) -1)
                            r (ash (s32 (peek32 (+ src (* 8 (+ i 1))))) -1))
                      (when (= i 0) (setf l0 l r0 r))
                      (poke32 (+ ring (logand (+ wr (* 2 i)) mask)) (logior (logand l #xFFFF) (ash (logand r #xFFFF) 16))))
             (when (or (> (abs (- l0 (first *audio-last*))) 2048) (> (abs (- r0 (second *audio-last*))) 2048))
               (incf *audio-seams*))
             (setf *audio-last* (list l r))
             (poke32 (+ *wa* #x40) (logand (+ wr (* 2 n)) #xFFFFFFFF))   ; written, after the samples
             n))))
    (t
     (let ((q (modus-int::w32-audio-queued *speaker*)))
       (when (and *audio-started* (zerop q)) (incf *audio-gaps*))
       (setf *audio-least* (if *audio-least* (min *audio-least* q) q)))
     (unless (and *pcm* (= (length *pcm*) n)) (setf *pcm* (make-array n :initial-element 0)))
     (dotimes (i n)
       (setf (aref *pcm* i) (ash (s32 (peek32 (+ src (* 8 i)))) -1)))
     (when (>= n 2)
       (when (or (> (abs (- (aref *pcm* 0) (first *audio-last*))) 2048)
                 (> (abs (- (aref *pcm* 1) (second *audio-last*))) 2048))
         (incf *audio-seams*))
       (setf *audio-last* (list (aref *pcm* (- n 2)) (aref *pcm* (- n 1)))))
     (if (modus-int::w32-audio-write *speaker* *pcm*)
         (progn (setf *audio-started* t) n)
         (progn (incf *audio-dropped*) 0)))))

;;; STATS, once a second: frames drawn, the longest frame (from one frame clock tick to the next), and the speaker's
;;; gaps, drops and least queued.  Logged every second with VESSEL_STATS set; otherwise only a second with a gap or a
;;; drop is.


(defun stats-report ()
  (let ((gaps (- (audio-gap-count) (first *audio-said*))) (drops (- *audio-dropped* (second *audio-said*))))
    (when (or *stats* (plusp gaps) (plusp drops) (plusp *audio-seams*))
      (format t "~&vessel-windows: ~D fps, ~D shown twice (DXGI), longest work ~D ms (to Present), ~D ms (with it), wait ~D ms, messages ~D ms; ~D collection~:P~@[; audio: ~A~]~%"
              *stat-frames* *stat-repeats* (round (* 1000 *stat-work*) internal-time-units-per-second)
              (round (* 1000 *stat-longest*) internal-time-units-per-second)
              (round (* 1000 *stat-wait*) internal-time-units-per-second)
              (round (* 1000 *stat-pump*) internal-time-units-per-second)
              (- (modus-int::%gc-count) (or *stat-gcs* (modus-int::%gc-count)))
              (and *speaker*
                   (format nil "~D gap~:P, ~D dropped, ~D jump~:P, least queued ~D ms~@[, device low ~D ms~]~@[, device clock ~@D ms~] (~D gaps in all)"
                           gaps drops *audio-seams* (if *audio-least* (round (* 1000 *audio-least*) 48000) 0)
                           (and (wasapi-p) (prog1 (round (* 1000 (peek64 (+ *wa* #x60))) 48000)
                                             (poke64 (+ *wa* #x60) #xFFFFFFFF)))
                           (and (wasapi-p) (let ((d (device-drift))) (and d (round d))))
                           (audio-gap-count))))
      (finish-output))
    (setf *audio-said* (list (audio-gap-count) *audio-dropped*) *audio-least* nil *stat-frames* 0 *stat-longest* 0
          *audio-seams* 0 *stat-wait* 0 *stat-pump* 0 *stat-late* 0 *stat-early* 0 *stat-work* 0 *stat-repeats* 0
          *stat-gcs* (modus-int::%gc-count)))
  (after 1 #'stats-report))

(defun audio-queued ()
  "Samples queued and not yet played: the ring's and the device's (WASAPI), or waveOut's."
  (cond ((null *speaker*) 0)
        ((wasapi-p) (+ (floor (ring-filled) 2) (* 2 (peek64 (+ *wa* #x50)))))
        (t (* 2 (modus-int::w32-audio-queued *speaker*)))))

;;; ---- SWIFT-CALL, answered --------------------------------------------------------------------------------------------

(defvar *swift* (make-hash-table :test 'equal))

(defmacro defswift (name args &body body)
  `(setf (gethash ,name *swift*)
         (lambda (,@(when args `(&optional ,@(loop for a in args collect `(,a 0)))) &rest more)
           (declare (ignore more) ,@(when args `((ignorable ,@args))))
           ,@body)))

;; the window
(defswift "window_title" (a n) (c *set-window-text* *hwnd* (wide1 (utf8-text a n))) 0)
(defswift "window_chrome" (style) 0)
(defswift "window_size" (w h)
  (let ((r (rect-area)))
    (zero r 16) (poke32 (+ r 8) (px w)) (poke32 (+ r 12) (px h))
    (c *adjust-rect* r +main-style+ (if (zerop *menu-bar*) 0 1) 0)
    (c *set-window-pos* *hwnd* 0 0 0 (- (s32 (peek32 (+ r 8))) (s32 (peek32 r)))
       (- (s32 (peek32 (+ r 12))) (s32 (peek32 (+ r 4)))) #x16))                            ; NOMOVE NOZORDER NOACTIVATE
  0)
(defswift "window_min_size" (w h) 0)
(defswift "window_new" (a n w h palette) (window-new a n w h palette))
(defswift "window_show" (win)
  (u *show-window* (ctl-hwnd (ctl-of win)) (if (second (ctl-data (ctl-of win))) 4 5)) 0)
(defswift "window_hide" (win) (u *show-window* (ctl-hwnd (ctl-of win)) 0) 0)
(defswift "window_retitle" (win a n) (c *set-window-text* (ctl-hwnd (ctl-of win)) (wide1 (utf8-text a n))) 0)
(defswift "window_fullscreen" (on) 0)
(defswift "window_remember" (a n) 0)
(defswift "window_pointer_lock" (on)
  (setf *lock-wanted* (/= on 0))
  (if *lock-wanted* (lock-pointer) (unlock-pointer))
  0)
(defswift "window_pointer_shape" (i)
  (setf *cursor* (c *load-cursor* 0 (aref #(32512 32513 32649 32649 32646 32515 32644 32645 32648) (min i 8))))
  (unless *locked* (poke64 (+ *page* #x40) *cursor*) (u *set-cursor* *cursor*))
  0)

;; controls
(defswift "ui_label_new" (a n x y w h) (label-new a n x y w h))
(defswift "ui_button_new" (a n x y w h) (button-new a n x y w h))
(defswift "ui_slider_new" (v x y w h) (slider-new v x y w h))
(defswift "ui_set_text" (h a n) (set-text h a n))
(defswift "ui_set_value" (h v) (set-value h v))
(defswift "ui_set_frame" (h x y w hh)
  (c *set-window-pos* (ctl-hwnd (ctl-of h)) 0 (px x) (px y) (px w) (px hh) #x14) 0)
(defswift "ui_add" (parent child)
  (let ((ch (ctl-of child)))
    (if (eq (ctl-kind ch) :view)                  ; a GPU view: drawn in the parent's window, filling it
        (view-attach (view-of child) (ctl-hwnd (ctl-of parent)))
        (u *set-parent* (ctl-hwnd ch) (ctl-hwnd (ctl-of parent)))))
  0)
(defswift "ui_remove" (h)
  (let ((ctl (ctl-of h)))
    (unless (zerop (ctl-hwnd ctl)) (remhash (ctl-hwnd ctl) *by-hwnd*) (u *destroy-window* (ctl-hwnd ctl)))
    (remhash h *ctls*))
  0)
(defswift "ui_send_tap" (h) (ui-event (list :tap h)) 0)
(defswift "ui_resign" () (u *set-focus* *hwnd*) 0)
(defswift "ui_events_next" (buf cap)
  (let ((e (first *ui-events*)))
    (if (null e)
        0
        (let ((n (cl-user::term-size e)))
          (if (> n cap)
              0
              (progn (pop *ui-events*) (cl-user::term-encode e buf) n))))))
(defswift "ui_alert_new" (ta tn ma mn sheet)
  (new-handle (make-ctl :kind :alert :data (list (utf8-text ta tn) (utf8-text ma mn) '()))))
(defswift "ui_alert_action" (h a n)
  (let ((d (ctl-data (ctl-of h)))) (setf (third d) (append (third d) (list (utf8-text a n))))) 0)
(defswift "ui_alert_present" (h) (alert-present h))
(defswift "ui_symbol_button_new" (a n x y w h) (button-new a n x y w h))
(defswift "ui_symbol_button_set" (h on) 0)
(defswift "glass_new" (x y w h radius tint) (panel-new x y w h radius))
(defswift "glass_button" (h on) 0)

;; menus and the status item
(defswift "menu_new" (a n in-bar) (menu-new a n in-bar))
(defswift "menu_item_new" (menu a n ka kn) (menu-item-new menu a n ka kn))
(defswift "menu_separator" (menu) (c *append-menu* (ctl-data (ctl-of menu)) #x800 0 0) 0)
(defswift "menu_item_set" (item checked enabled) (menu-item-set item checked enabled))
(defswift "status_item_new" (a n sa sn)
  (setf *tray-tip* (let ((s (utf8-text a n))) (if (plusp (length s)) s "Hello 3D")))
  (tray-add)
  (setf *status-item* (new-handle (make-ctl :kind :status))))
(defswift "status_item_menu" (item menu) (setf *status-menu* (ctl-data (ctl-of menu))) 0)
(defswift "status_item_title" (item a n) (tray-tip (utf8-text a n)) 0)
(defswift "mac_on_quit" (on) (setf *on-quit* (/= on 0)) 0)
(defswift "mac_quit_reply" () (quit-now))
(defswift "dock_badge" (a n) 0)
(defswift "dock_bounce" (critical) 0)

;; notifications: the notification area's balloons, which Windows shows as its own notifications
(defswift "notify_permission" () 1)
(defswift "notify_ask" () (ui-event (list :notifyask 0 1)) 0)
(defswift "notify_post" (ta tn ba bn after)
  (let ((title (utf8-text ta tn)) (body (utf8-text ba bn))
        (h (new-handle (make-ctl :kind :notification))))
    (after after (lambda () (setf *last-notification* h) (tray-balloon title body)))
    h))
(defswift "notify_badge" (count) 0)

;; spatial sound: not here (sound.lisp pans instead)
(defswift "spatial_start" () 0)
(defswift "spatial_head_permission" () 3)
(defswift "spatial_head_attitude" (a) 0)

;; the phone's motion and the watch's link: neither is here (a PC)
(defswift "device_motion" (on) 0)
(defswift "device_attitude" (a) 0)
(defswift "device_recenter" () 0)
(defswift "link_open" () 0)
(defswift "link_state" () 0)
(defswift "link_send" (a n) 0)

;; where this PC is: Windows' Geolocator (WinRT)
(defswift "location_permission" () *location-permission*)
(defswift "location_ask" () (location-ask) 0)
(defswift "location_start" () (location-start) 0)
(defswift "location_stop" () 0)
(defswift "location_get" (a)
  (if (null *fix*)
      0
      (progn (put-floats a *fix*) 1)))

;; game controllers
(defswift "pad_count" () (pad-count))
(defswift "pad_name" (i a cap) (if (xinput-state i) (put-utf8 "Xbox controller" a cap) 0))
(defswift "pad_state" (i a n) (pad-state i a n))
(defswift "pad_light" (i rgb) 0)
(defswift "pad_rumble" (i strength ms) (pad-rumble i strength ms))

;; the GPU
(defswift "gpu_language" () 1)
(defswift "gpu_view_new" (x y w h)
  (let ((v (view-new)))
    (when *debug* (format t "~&vessel-windows: gpu_view_new -> ~S (next handle ~S)~%" v *next-handle*))
    v))
(defswift "gpu_view_size" (view) (view-size view))
(defswift "gpu_view_print" (view) (setf (view-print (view-of view)) t) 0)
(defswift "gpu_view_hdr" (view on) 0)             ; standard range: see the top of this file
(defswift "gpu_headroom" () 1000)
(defswift "gpu_shader_new" (a n) (shader-new a n))
(defswift "gpu_pipeline_new" (s va vn fa fn) (pipeline-new s va vn fa fn))
(defswift "gpu_buffer_new" (a n) (buffer-new a n))
(defswift "gpu_buffer_write" (b off a n) (buffer-write (first (gpu-data b)) off a n))
(defswift "gpu_pass_begin" (view clear) (pass-begin view clear))
(defswift "gpu_pass_end" (pass) (pass-end pass))
(defswift "gpu_set_pipeline" (pass p)
  (destructuring-bind (vs ps) (gpu-data p)
    (m *ctx* 11 vs 0 0)
    (m *ctx* 9 ps 0 0)
    (m *ctx* 35 *blend* 0 #xFFFFFFFF))
  0)
(defswift "gpu_set_buffer" (pass b index stage)
  (let ((srv (second (gpu-data b))))
    (unless (= stage 1) (m *ctx* 25 index 1 (ptr-cell 7 srv)))
    (unless (= stage 0) (m *ctx* 8 index 1 (ptr-cell 7 srv))))
  0)
(defswift "gpu_draw" (pass prim start count)
  (first-vertex start)
  (m *ctx* 24 (topology prim))
  (m *ctx* 13 count start)
  0)
(defswift "gpu_draw_indexed" (pass prim indices count first base)
  (first-vertex base)
  (m *ctx* 19 (first (gpu-data indices)) 57 0)    ; R16_UINT
  (m *ctx* 24 (topology prim))
  (m *ctx* 12 count first base)
  0)
(defswift "gpu_set_depth" (pass mode) (m *ctx* 36 (aref *depths* mode) 0) 0)
(defswift "gpu_set_cull" (pass mode) (m *ctx* 43 (aref *rasters* mode)) 0)
(defswift "gpu_set_bytes" (pass a n index stage) (set-bytes a n index stage))
(defswift "gpu_texture_new" (a w h) (texture-new a w h))
(defswift "gpu_set_texture" (pass tex index stage)
  (m *ctx* 8 (+ 8 index) 1 (ptr-cell 7 (second (gpu-data tex))))
  (m *ctx* 10 index 1 (ptr-cell 5 *sampler*))
  0)

(defvar *missing* (make-hash-table :test 'equal))

(defun host-call (name &rest args)
  "SWIFT-CALL on Windows: the host's NAME with ARGS, directly -- no frame, no address (INSTALL-BRIDGE)."
  (let ((fn (gethash name *swift*)))
    (unwind-protect
         (if (null fn)
             (progn (unless (gethash name *missing*)
                      (setf (gethash name *missing*) t)
                      (format t "~&vessel-windows: no ~A here~%" name))
                    (error "swift-call: no Swift function named ~A" name))
             (let ((r (handler-case (apply fn (mapcar (lambda (a) (or a 0)) args))
                        (error (e) (error "~A ~{~D~^ ~}: ~A" name args e)))))
               (if (integerp r) r 0)))
      (setf (fill-pointer *tokens*) 0))))

(defun install-bridge ()
  "After ios-swift.lisp: its calls go straight to this host, a vector is passed as a token for itself (%ADDR), and an
   event is the host's own term (%NEXT-EVENT) -- nothing of Lisp's is reached through an address a collection can move."
  (setf (fdefinition (find-symbol "SWIFT-CALL" "KILN-SWIFT")) #'host-call
        (fdefinition (find-symbol "%ADDR" "KILN-SWIFT")) #'token
        (fdefinition (find-symbol "%NEXT-EVENT" "KILN-SWIFT")) (lambda () (pop *ui-events*))))

(defun swift (frame)
  "SWIFT-CALL's frame: +0 the argument count, +8.. the arguments, +64 the result, +128 the name."
  (let* ((nargs (min 6 (peek64 frame)))
         (args (loop for i below nargs collect (peek64 (+ frame 8 (* 8 i)))))
         (name (with-output-to-string (s)
                 (loop for i from 128 below 256
                       for b = (peek8 (+ frame i))
                       until (zerop b) do (write-char (code-char b) s))))
         (fn (gethash name *swift*)))
    (if (null fn)
        (progn (unless (gethash name *missing*)
                 (setf (gethash name *missing*) t)
                 (format t "~&vessel-windows: no ~A here~%" name))
               -1)
        (let ((r (handler-case (apply fn args)
                   (error (e)
                     (error "~A ~{~D~^ ~}: ~A" name args e)))))
          (poke64 (+ frame 64) (if (integerp r) r 0))
          0))))

;;; ---- %KILN-SYS ----------------------------------------------------------------------------------------------------

(defun sys (n a b cc)
  (case n
    (1001 (case a (0 (client-size) *w*) (1 (client-size) *h*) (2 (ui-scale)) ((3 4 5 6) 0) (t -22)))
    ((1002 1003 1005 1006 1007) 0)                  ; the framebuffer calls: Direct3D draws this window
    (1004 (next-event))
    (1008 (wait-frame a))
    (1010 (audio-open a b))
    (1011 (audio-write a b))
    (1012 (audio-queued))
    (1013 (audio-gap-count))
    (1060 (swift a))
    (t -38)))

(defun cl-user::%kiln-sys (n a b c &optional (d 0) (e 8))
  (declare (ignore d e))
  (sys n a b c))
