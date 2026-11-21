;;;; hello3d.lisp -- hello, world, in 3D: a small scene the GPU draws every frame, flown through with the keyboard and
;;;; the mouse, with the platform's own controls over it.  Everything is Lisp: the geometry, the textures (made
;;;; here, pixel by pixel), the matrices, the frame loop; Metal draws it (gpu.lisp, through modus's GPU.swift).
;;;;
;;;;   a ring of turning cubes round a bobbing one, on a checkered floor, lit from one side, fading into the dusk
;;;;   W A S D  move   Q E (or Shift Space)  down and up   the arrows  turn
;;;;   click     look around with the mouse (the pointer is held)     Esc  let the pointer go
;;;;   the native controls: the frame rate, spin on or off, and how fast
;;;;
;;;; Its own app, not the desk's: kiln's boot/hello3d-world.lisp loads it and `kiln mac --app=hello3d' builds it.
;;;; MAIN is the whole program: the window's events and the frame clock are read here, as the desk reads them.

(defpackage #:hello3d
  (:use #:cl)
  (:export #:main))

(in-package #:hello3d)

(defun %sys (n a b c) (cl-user::%kiln-sys n a b c))

;;; ---- the shader ---------------------------------------------------------------------------------------------

(defparameter *shader* "
#include <metal_stdlib>
using namespace metal;

struct Vertex { packed_float3 position; packed_float3 normal; packed_float2 uv; };
struct Frame  { float4x4 view_projection; float4 light; float4 eye; float4 fog; float4 fog_colour; };
struct Object { float4x4 model; float4 tint; };
struct Out    { float4 position [[position]]; float3 normal; float2 uv; float3 world; };

vertex Out world_vertex(uint id [[vertex_id]], device const Vertex *vertices [[buffer(0)]],
                        constant Frame &frame [[buffer(1)]], constant Object &object [[buffer(2)]]) {
    Vertex v = vertices[id];
    float4 world = object.model * float4(float3(v.position), 1.0);
    Out out;
    out.position = frame.view_projection * world;
    out.world = world.xyz;
    out.normal = (object.model * float4(float3(v.normal), 0.0)).xyz;
    out.uv = float2(v.uv);
    return out;
}

fragment float4 world_fragment(Out in [[stage_in]], constant Frame &frame [[buffer(1)]],
                               constant Object &object [[buffer(2)]],
                               texture2d<float> picture [[texture(0)]], sampler s [[sampler(0)]]) {
    float3 n = normalize(in.normal);
    float sun = max(dot(n, -normalize(frame.light.xyz)), 0.0);
    float sky = 0.5 + 0.5 * n.y;                                   // light from above, a little from everywhere
    float3 base = picture.sample(s, in.uv).rgb * object.tint.rgb;
    float3 colour = base * (0.18 + 0.22 * sky + 0.75 * sun);
    float d = distance(in.world, frame.eye.xyz);
    float fog = clamp((d - frame.fog.x) / (frame.fog.y - frame.fog.x), 0.0, 1.0);
    return float4(mix(colour, frame.fog_colour.rgb, fog), 1.0);
}
")

;;; ---- vectors and matrices: single-floats, the matrices column-major as Metal reads them ---------------------------

(deftype mat () '(simple-array single-float (16)))

(defun mat () (make-array 16 :element-type 'single-float :initial-element 0.0))

(defun identity-mat ()
  (let ((m (mat))) (setf (aref m 0) 1.0 (aref m 5) 1.0 (aref m 10) 1.0 (aref m 15) 1.0) m))

(defun mat* (a b)
  "A times B: B is applied first."
  (let ((c (mat)))
    (dotimes (col 4 c)
      (dotimes (row 4)
        (let ((s 0.0))
          (dotimes (k 4) (incf s (* (aref a (+ (* k 4) row)) (aref b (+ (* col 4) k)))))
          (setf (aref c (+ (* col 4) row)) s))))))

(defun translation (x y z)
  (let ((m (identity-mat))) (setf (aref m 12) (float x 1.0) (aref m 13) (float y 1.0) (aref m 14) (float z 1.0)) m))

(defun scaling (s)
  (let ((m (mat)) (s (float s 1.0))) (setf (aref m 0) s (aref m 5) s (aref m 10) s (aref m 15) 1.0) m))

(defun rotation-y (a)
  (let ((m (identity-mat)) (c (float (cos a) 1.0)) (s (float (sin a) 1.0)))
    (setf (aref m 0) c (aref m 8) s (aref m 2) (- s) (aref m 10) c) m))

(defun rotation-x (a)
  (let ((m (identity-mat)) (c (float (cos a) 1.0)) (s (float (sin a) 1.0)))
    (setf (aref m 5) c (aref m 9) (- s) (aref m 6) s (aref m 10) c) m))

(defun perspective (fovy aspect near far)
  "Right-handed, looking down -Z, depth 0 at NEAR to 1 at FAR (Metal's)."
  (let ((m (mat)) (f (/ 1.0 (tan (/ fovy 2.0)))))
    (setf (aref m 0) (float (/ f aspect) 1.0)
          (aref m 5) (float f 1.0)
          (aref m 10) (float (/ far (- near far)) 1.0)
          (aref m 11) -1.0
          (aref m 14) (float (/ (* near far) (- near far)) 1.0))
    m))

(defun look (eye yaw pitch)
  "The view from EYE (x y z) facing YAW (about the vertical, 0 down -Z) and PITCH (up positive)."
  (destructuring-bind (ex ey ez) eye
    (let* ((cp (cos pitch))
           (fx (* -1 (sin yaw) cp)) (fy (sin pitch)) (fz (* -1 (cos yaw) cp))
           ;; right = forward x up(0 1 0), normalised; up = right x forward
           (rx (- fz)) (rz fx) (rl (sqrt (+ (* rx rx) (* rz rz))))
           (rx (/ rx rl)) (ry 0.0) (rz (/ rz rl))
           (ux (- (* ry fz) (* rz fy))) (uy (- (* rz fx) (* rx fz))) (uz (- (* rx fy) (* ry fx)))
           (m (mat)))
      (flet ((put (i v) (setf (aref m i) (float v 1.0))))
        (put 0 rx) (put 4 ry) (put 8 rz) (put 12 (- (+ (* rx ex) (* ry ey) (* rz ez))))
        (put 1 ux) (put 5 uy) (put 9 uz) (put 13 (- (+ (* ux ex) (* uy ey) (* uz ez))))
        (put 2 (- fx)) (put 6 (- fy)) (put 10 (- fz)) (put 14 (+ (* fx ex) (* fy ey) (* fz ez)))
        (put 15 1.0))
      m)))

(defun floats (&rest parts)
  "One single-float vector of PARTS, each a number or a vector of them."
  (let ((out (make-array (loop for p in parts sum (if (numberp p) 1 (length p))) :element-type 'single-float))
        (i 0))
    (dolist (p parts out)
      (if (numberp p)
          (progn (setf (aref out i) (float p 1.0)) (incf i))
          (loop for x across p do (setf (aref out i) (float x 1.0)) (incf i))))))

;;; ---- geometry: position, normal, uv -- eight floats a vertex -----------------------------------------------------

(defun cube-mesh ()
  "A unit cube's 24 vertices (four a face, each face its own normal) and 36 indices, counter-clockwise outside."
  (let ((verts '()) (indices '()) (n 0))
    ;; each face: its normal and two axes U V with U x V = normal
    (dolist (face '(((1 0 0) (0 0 -1) (0 1 0)) ((-1 0 0) (0 0 1) (0 1 0))
                    ((0 1 0) (1 0 0) (0 0 -1)) ((0 -1 0) (1 0 0) (0 0 1))
                    ((0 0 1) (1 0 0) (0 1 0)) ((0 0 -1) (-1 0 0) (0 1 0))))
      (destructuring-bind (nrm u v) face
        (loop for (su sv tu tv) in '((-1 -1 0 1) (1 -1 1 1) (1 1 1 0) (-1 1 0 0))
              do (dotimes (k 3)
                   (push (* 0.5 (+ (nth k nrm) (* su (nth k u)) (* sv (nth k v)))) verts))
                 (dolist (x nrm) (push x verts))
                 (push tu verts) (push tv verts))
        (dolist (i '(0 1 2 0 2 3)) (push (+ n i) indices))
        (incf n 4)))
    (values (apply #'floats (nreverse verts)) (nreverse indices))))

(defun floor-mesh (size tiles)
  "A square of SIZE on y = 0, facing up, its texture repeated TILES times across."
  (let ((h (/ size 2.0)) (tl (float tiles 1.0)))
    (values (floats (- h) 0 h    0 1 0  0 tl
                    h 0 h        0 1 0  tl tl
                    h 0 (- h)    0 1 0  tl 0
                    (- h) 0 (- h) 0 1 0  0 0)
            '(0 1 2 0 2 3))))

;;; ---- textures, made here -------------------------------------------------------------------------------------------

(defun texture-pixels (size fn)
  "SIZE x SIZE RGBA pixels, (FN x y) giving each one's (values r g b) in 0..255."
  (let ((px (make-array (* size size 4) :element-type '(unsigned-byte 8))))
    (dotimes (y size px)
      (dotimes (x size)
        (multiple-value-bind (r g b) (funcall fn x y)
          (let ((i (* 4 (+ x (* y size)))))
            (setf (aref px i) r (aref px (+ i 1)) g (aref px (+ i 2)) b (aref px (+ i 3)) 255)))))))

(defun checker (x y)
  "The floor: a two-tone check with a fine line between."
  (let ((line (or (< (mod x 128) 3) (< (mod y 128) 3)))
        (dark (oddp (+ (floor x 128) (floor y 128)))))
    (cond (line (values 210 200 170))
          (dark (values 70 92 78))
          (t (values 104 128 106)))))

(defun crate (x y)
  "A cube's face: a light border, and within it a soft gradient and a diagonal."
  (let* ((edge (min x y (- 127 x) (- 127 y)))
         (diag (< (abs (- x y)) 4)))
    (cond ((< edge 8) (values 235 235 228))
          (diag (values 210 210 205))
          (t (let ((v (+ 120 (floor (* 80 (+ x y)) 254)))) (values v v v))))))

;;; ---- the game ------------------------------------------------------------------------------------------------------

(defstruct game
  view pipeline
  cube-vertices cube-indices floor-vertices floor-indices
  crate-texture floor-texture
  (eye (list 0.0 1.6 9.0)) (yaw 0.0) (pitch -0.08)
  (held (make-hash-table)) (locked nil)
  (spin t) (speed 1.0) (angle 0.0)
  fps-label (frames 0) (fps-at 0))

(defun hue (h)
  "A bright colour at hue H (0..1), as a tint."
  (flet ((c (o) (let ((x (abs (- (mod (+ h o) 1.0) 0.5)))) (min 1.0 (max 0.25 (- (* 3 x) 0.25))))))
    (floats (c 0.0) (c (/ 1 3.0)) (c (/ 2 3.0)) 1.0)))

(defun key-down-p (g &rest keysyms)
  (some (lambda (k) (gethash k (game-held g))) keysyms))

(defun steer (g dt)
  "Move the eye by the keys held: forward and back along where it faces (level), sideways, up and down, and turn."
  (let* ((yaw (game-yaw g)) (step (* 4.0 dt)) (turn (* 1.6 dt))
         (fx (- (sin yaw))) (fz (- (cos yaw)))          ; forward, level
         (rx (cos yaw)) (rz (- (sin yaw)))              ; right
         (dx 0.0) (dy 0.0) (dz 0.0))
    (when (key-down-p g #x77 #x57) (incf dx fx) (incf dz fz))                  ; w
    (when (key-down-p g #x73 #x53) (decf dx fx) (decf dz fz))                  ; s
    (when (key-down-p g #x64 #x44) (incf dx rx) (incf dz rz))                  ; d
    (when (key-down-p g #x61 #x41) (decf dx rx) (decf dz rz))                  ; a
    (when (key-down-p g #x65 #x45 #x20) (incf dy 1.0))                         ; e, space
    (when (key-down-p g #x71 #x51 #xffe1 #xffe2) (decf dy 1.0))                ; q, shift
    (when (key-down-p g #xff51) (incf (game-yaw g) turn))                      ; left
    (when (key-down-p g #xff53) (decf (game-yaw g) turn))                      ; right
    (when (key-down-p g #xff52) (incf (game-pitch g) turn))                    ; up
    (when (key-down-p g #xff54) (decf (game-pitch g) turn))                    ; down
    (setf (game-pitch g) (max -1.45 (min 1.45 (game-pitch g))))
    (destructuring-bind (x y z) (game-eye g)
      (setf (game-eye g) (list (+ x (* step dx)) (max 0.3 (+ y (* step dy))) (+ z (* step dz)))))))

(defun %signed20 (v) (if (logbitp 19 v) (- v #x100000) v))

(defun handle-event (g e)
  "One event from the window: keys held and let go, the pointer locked by a click and let go by Esc, the mouse's
   motion turning the eye while it is locked."
  (let ((type (logand (ash e -40) #xFF)))
    (case type
      (8 (let ((ks (logand e #xFFFFFFFFFF)))
           (setf (gethash ks (game-held g)) t)
           (when (and (= ks #xff1b) (game-locked g))
             (kiln-mac:window-pointer-lock nil) (setf (game-locked g) nil))))
      (9 (remhash (logand e #xFFFFFFFFFF) (game-held g)))
      (1 (unless (game-locked g)
           (kiln-mac:window-pointer-lock t) (setf (game-locked g) t)))
      (13 (when (game-locked g)
            (let ((dx (%signed20 (logand e #xFFFFF))) (dy (%signed20 (logand (ash e -20) #xFFFFF))))
              (decf (game-yaw g) (* 0.0025 dx))
              (setf (game-pitch g) (max -1.45 (min 1.45 (- (game-pitch g) (* 0.0025 dy)))))))))))

(defun draw-thing (pass vertices indices count texture model tint)
  "One object: its vertices, texture, model matrix and tint, drawn."
  (kiln-gpu:gpu-bind pass vertices 0)
  (kiln-gpu:gpu-bind-texture pass texture 0)
  (kiln-gpu:gpu-constants pass (floats model tint) 2)
  (kiln-gpu:gpu-draw-indexed pass indices count))

(defparameter *dusk* #x1d2a3c "The sky, and the fog the far things fade into.")

(defun render (g)
  (multiple-value-bind (w h) (kiln-gpu:gpu-view-size (game-view g))
    (when (and (plusp w) (plusp h))
      (let* ((projection (perspective (/ pi 3) (/ w h) 0.1 120.0))
             (view (look (game-eye g) (game-yaw g) (game-pitch g)))
             (frame (floats (mat* projection view)
                            -0.5 -1.0 -0.35 0.0
                            (first (game-eye g)) (second (game-eye g)) (third (game-eye g)) 1.0
                            18.0 60.0 0.0 0.0
                            (/ (ldb (byte 8 16) *dusk*) 255.0) (/ (ldb (byte 8 8) *dusk*) 255.0)
                            (/ (ldb (byte 8 0) *dusk*) 255.0) 1.0))
             (a (game-angle g)))
        (kiln-gpu:with-gpu-pass (pass (game-view g) :clear *dusk*)
          (kiln-gpu:gpu-use pass (game-pipeline g))
          (kiln-gpu:gpu-cull pass :back)
          (kiln-gpu:gpu-constants pass frame 1)
          ;; the floor
          (draw-thing pass (game-floor-vertices g) (game-floor-indices g) 6 (game-floor-texture g)
                      (identity-mat) (floats 1 1 1 1))
          ;; the ring: eight cubes, each its own colour, turning as it goes round
          (dotimes (i 8)
            (let ((around (+ (* i (/ pi 4)) (* 0.25 a))))
              (draw-thing pass (game-cube-vertices g) (game-cube-indices g) 36 (game-crate-texture g)
                          (mat* (translation (* 6 (sin around)) 1.0 (* 6 (cos around)))
                                (mat* (rotation-y (+ a (* i 0.7))) (rotation-x (* 0.6 a))))
                          (hue (/ i 8.0)))))
          ;; and in the middle, a bigger one, bobbing
          (draw-thing pass (game-cube-vertices g) (game-cube-indices g) 36 (game-crate-texture g)
                      (mat* (translation 0 (+ 2.2 (* 0.35 (sin (* 1.3 a)))) 0)
                            (mat* (rotation-y (* -0.5 a)) (scaling 1.8)))
                      (floats 1.0 0.85 0.55 1)))))))

(defun make-hud (g)
  "The platform's own controls over the picture."
  (kiln-swift:ui-label "Hello, world" 20 16 300 30)
  (setf (game-fps-label g) (kiln-swift:ui-label "" 20 46 300 22))
  (kiln-swift:ui-label "click: look around   Esc: let go   WASD, QE: move   arrows: turn" 20 70 600 22)
  (let ((spin (kiln-swift:ui-button "spin: on" 20 100 110 32))
        (speed (kiln-swift:ui-slider 50 140 104 200 24)))
    (kiln-swift:on-tap spin (lambda (value h)
                              (declare (ignore value h))
                              (setf (game-spin g) (not (game-spin g)))
                              (kiln-swift:ui-set-text spin (if (game-spin g) "spin: on" "spin: off"))))
    (kiln-swift:on-event :slider speed (lambda (value h)
                                         (declare (ignore h))
                                         (setf (game-speed g) (/ value 50.0))))))

(defun start ()
  "Everything made once: the view filling the window, the pipeline, the meshes, the textures, the controls."
  (let ((g (make-game)))
    (setf (game-view g) (kiln-gpu:gpu-view 0 0 0 0))
    (let ((shader (kiln-gpu:gpu-shader *shader*)))
      (setf (game-pipeline g) (and shader (kiln-gpu:gpu-pipeline shader "world_vertex" "world_fragment"))))
    (unless (game-pipeline g) (error "hello3d: the shader or its pipeline did not build (see the log)"))
    (multiple-value-bind (v i) (cube-mesh)
      (setf (game-cube-vertices g) (kiln-gpu:gpu-buffer v) (game-cube-indices g) (kiln-gpu:gpu-indices i)))
    (multiple-value-bind (v i) (floor-mesh 120 60)
      (setf (game-floor-vertices g) (kiln-gpu:gpu-buffer v) (game-floor-indices g) (kiln-gpu:gpu-indices i)))
    (setf (game-crate-texture g) (kiln-gpu:gpu-texture (texture-pixels 128 #'crate) 128 128)
          (game-floor-texture g) (kiln-gpu:gpu-texture (texture-pixels 256 #'checker) 256 256))
    (make-hud g)
    (setf (game-fps-at g) (get-internal-real-time))
    g))

(defun main ()
  "Hello, world in 3D: start, then a frame each time the screen refreshes."
  (%sys 1002 0 (+ (%sys 1001 0 0 0) (* (%sys 1001 1 0 0) 65536)) *dusk*)   ; under the view, until it draws
  (%sys 1003 0 0 0)
  (let ((g (start)) (last (get-internal-real-time)))
    (format t "~&hello3d: running~%")
    (loop
      (%sys 1008 34 0 0)                                ; the screen's next refresh
      (loop for e = (%sys 1004 0 0 0) until (zerop e) do (handle-event g e))
      (kiln-swift:swift-pump)                           ; the native controls' events
      (let* ((now (get-internal-real-time))
             (dt (min 0.1 (/ (- now last) internal-time-units-per-second))))
        (setf last now)
        (steer g dt)
        (when (game-spin g) (incf (game-angle g) (* dt (game-speed g))))
        (handler-case (render g)
          (serious-condition (c) (format t "~&hello3d: ~A~%" c)))
        ;; the frame rate, twice a second
        (incf (game-frames g))
        (let ((since (/ (- now (game-fps-at g)) internal-time-units-per-second)))
          (when (>= since 0.5)
            (kiln-swift:ui-set-text (game-fps-label g)
                                    (format nil "~D frames a second" (round (game-frames g) since)))
            (setf (game-frames g) 0 (game-fps-at g) now)))))))
