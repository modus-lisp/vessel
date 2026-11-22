;;;; hello3d.lisp -- hello, world, in 3D: a small scene the GPU draws every frame, flown through with the keyboard and
;;;; the mouse, with the platform's own controls over it.  Everything is Lisp: the geometry, the textures (made
;;;; here, pixel by pixel), the matrices, the frame loop; Metal draws it (gpu.lisp, through modus's GPU.swift).
;;;;
;;;;   a ring of turning cubes round a bobbing one, on a checkered floor, lit from one side, fading into the dusk
;;;;   W A S D  move   Q E (or Shift Space)  down and up   the arrows  turn
;;;;   click     look around with the mouse (the pointer is held)     Esc  let the pointer go
;;;;   a game controller: the left stick moves, the right looks, the triggers lift and lower, A turns spin on and off
;;;;   on a phone (MAIN :TOUCH T): the left thumb a stick where it lands, the right thumb drags the view
;;;;   and every cube hums its own note, placed where it is: in 3D through the platform's binaural renderer (with
;;;;   AirPods, still in the world as your head turns), or else panned left and right
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
  (spin t) (speed 0.4) (angle 0.0)
  fps-label (frames 0) (fps-at 0)
  ;; a game controller: its state, read each frame (gamepad.lisp); A as it was, to see it pressed; the HUD's line
  (pad (make-array 19 :element-type 'single-float :initial-element 0.0)) (pad-a nil) pad-label (pad-seen -1)
  ;; sound (sound.lisp): a humming voice per cube, the ring's then the middle one's; a blip; on or off
  (hums '()) blip (sound t) (spatial nil) (head nil)
  ;; where each toggle is shown, to keep them in step: the HUD's buttons, and the menus' items (a Mac's)
  spin-button sound-button head-button speed-slider (spin-items '()) (sound-items '()) (head-items '()) status
  ;; TOUCH (a phone): each finger by its slot (type 14 events) -> (role x y) where it last was -- :move fingers
  ;; also keep where they landed, the stick's centre; the stick's deflection now, -1..1 each way
  (touch nil) (fingers (make-hash-table)) (stick-x 0.0) (stick-y 0.0) (scale 1))

;;; WHERE THE CUBES ARE, for the picture and the sound alike: the ring's Ith at ANGLE, and the middle one
(defun ring-position (i angle)
  (let ((around (+ (* i (/ pi 4)) (* 0.25 angle))))
    (values (* 6 (sin around)) 1.0 (* 6 (cos around)))))

(defun middle-height (angle) (+ 2.2 (* 0.35 (sin (* 1.3 angle)))))

(defparameter *ring-notes* '(220 247 294 330 370 440 494 587) "A pentatonic of A, a note a ring cube.")

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
    ;; THE TOUCH STICK: as WASD, by how far the left thumb is from where it landed
    (when (game-touch g)
      (incf dx (+ (* (game-stick-x g) rx) (* (- (game-stick-y g)) fx)))
      (incf dz (+ (* (game-stick-x g) rz) (* (- (game-stick-y g)) fz))))
    ;; A GAME CONTROLLER, the first one: the left stick moves (as WASD, and by how far it is pushed), the right
    ;; looks, the triggers lift and lower
    (when (kiln-pad:pad-read 0 (game-pad g))
      (let* ((p (game-pad g))
             (mx (kiln-pad:pad-axis p :left-x)) (my (kiln-pad:pad-axis p :left-y)))
        (incf dx (+ (* mx rx) (* my fx)))
        (incf dz (+ (* mx rz) (* my fz)))
        (incf dy (- (kiln-pad:pad-axis p :right-trigger) (kiln-pad:pad-axis p :left-trigger)))
        (decf (game-yaw g) (* 2.2 dt (kiln-pad:pad-axis p :right-x)))
        (incf (game-pitch g) (* 1.6 dt (kiln-pad:pad-axis p :right-y)))
        ;; A, pressed (not held): spin on or off, and a short buzz to say so
        (let ((a (kiln-pad:pad-button p :a)))
          (when (and a (not (game-pad-a g)))
            (set-spin g (not (game-spin g)))
            (blip g)
            (kiln-pad:pad-rumble 0 0.5 80))
          (setf (game-pad-a g) a))))
    (setf (game-pitch g) (max -1.45 (min 1.45 (game-pitch g))))
    (destructuring-bind (x y z) (game-eye g)
      (setf (game-eye g) (list (+ x (* step dx)) (max 0.3 (+ y (* step dy))) (+ z (* step dz)))))))

(defun %signed20 (v) (if (logbitp 19 v) (- v #x100000) v))

(defun touch-event (g e)
  "A finger's own touch (type 14): on the left half of the screen a stick, centred where it lands; on the right,
   dragged, the view turned."
  (let* ((x (logand e #xFFFFF)) (y (logand (ash e -20) #xFFFFF))
         (slot (logand (ash e -48) #xF)) (phase (logand (ash e -52) 3))
         (fingers (game-fingers g)) (scale (game-scale g))
         (radius (* 60 scale)))                            ; full deflection 60 points out
    (case phase
      (0 (setf (gethash slot fingers)
               (if (< x (floor (%sys 1001 0 0 0) 2)) (list :move x y x y) (list :look x y))))
      (1 (let ((f (gethash slot fingers)))
           (when f
             (if (eq (first f) :move)
                 (destructuring-bind (role cx cy &rest r) f
                   (declare (ignore role r))
                   (setf (game-stick-x g) (max -1.0 (min 1.0 (/ (- x cx) radius)))
                         (game-stick-y g) (max -1.0 (min 1.0 (/ (- y cy) radius)))))
                 (destructuring-bind (role lx ly) f
                   (declare (ignore role))
                   (decf (game-yaw g) (/ (* 0.005 (- x lx)) scale))
                   (setf (game-pitch g) (max -1.45 (min 1.45 (- (game-pitch g) (/ (* 0.005 (- y ly)) scale)))))
                   (setf (gethash slot fingers) (list :look x y)))))))
      (2 (let ((f (gethash slot fingers)))
           (when (and f (eq (first f) :move)) (setf (game-stick-x g) 0.0 (game-stick-y g) 0.0))
           (remhash slot fingers))))))

(defun handle-event (g e)
  "One event from the window: keys held and let go, the pointer locked by a click and let go by Esc, the mouse's
   motion turning the eye while it is locked."
  (let ((type (logand (ash e -40) #xFF)))
    (when (game-touch g)                    ; a phone: fingers are its input, and there is no pointer to hold
      (case type
        (14 (touch-event g e))
        ((8 9) nil)
        (t (return-from handle-event nil))))
    (case type
      (8 (let ((ks (logand e #xFFFFFFFFFF)))
           (setf (gethash ks (game-held g)) t)
           (when (and (= ks #xff1b) (game-locked g))
             (kiln-mac:window-pointer-lock nil) (setf (game-locked g) nil))))
      (9 (remhash (logand e #xFFFFFFFFFF) (game-held g)))
      (1 (unless (game-locked g)
           (kiln-mac:window-pointer-lock t) (setf (game-locked g) t) (blip g)))
      (13 (when (game-locked g)
            (let ((dx (%signed20 (logand e #xFFFFF))) (dy (%signed20 (logand (ash e -20) #xFFFFF))))
              (decf (game-yaw g) (* 0.0025 dx))
              (setf (game-pitch g) (max -1.45 (min 1.45 (- (game-pitch g) (* 0.0025 dy)))))))))))

(defun blip (g)
  (when (game-blip g) (kiln-sound:play (game-blip g) :gain 0.35)))

(defun hear (g)
  "Each cube's hum placed where it is now, heard from the eye -- in 3D by the platform, or panned here -- then the
   speaker fed (the blip's)."
  (when (game-hums g)
    (destructuring-bind (ex ey ez) (game-eye g)
      (let ((a (game-angle g)) (yaw (game-yaw g)))
        (when (game-spatial g) (kiln-sound:spatial-listener ex ey ez yaw (game-pitch g)))
        (loop for v in (game-hums g) for i from 0
              do (multiple-value-bind (x y z) (if (< i 8) (ring-position i a) (values 0 (middle-height a) 0))
                   (if (game-spatial g)
                       (kiln-sound:spatial-place v x y z :gain (if (< i 8) 0.35 0.5))
                       (multiple-value-bind (gain pan)
                           (kiln-sound:place ex ez yaw x y z :loudness (if (< i 8) 0.14 0.22))
                         (kiln-sound:voice-set v :gain gain :pan pan)))))))
    (kiln-sound:sound-pump)))

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
            (multiple-value-bind (x y z) (ring-position i a)
              (draw-thing pass (game-cube-vertices g) (game-cube-indices g) 36 (game-crate-texture g)
                          (mat* (translation x y z)
                                (mat* (rotation-y (+ a (* i 0.7))) (rotation-x (* 0.6 a))))
                          (hue (/ i 8.0)))))
          ;; and in the middle, a bigger one, bobbing
          (draw-thing pass (game-cube-vertices g) (game-cube-indices g) 36 (game-crate-texture g)
                      (mat* (translation 0 (middle-height a) 0)
                            (mat* (rotation-y (* -0.5 a)) (scaling 1.8)))
                      (floats 1.0 0.85 0.55 1)))))))

(defun set-spin (g on)
  "Spin on or off, shown wherever it is: the HUD's button, the menus' check marks."
  (setf (game-spin g) on)
  (when (game-spin-button g) (kiln-swift:ui-set-text (game-spin-button g) (if on "spin: on" "spin: off")))
  (dolist (i (game-spin-items g)) (kiln-mac:menu-check i on)))

(defun set-sound (g on)
  (setf (game-sound g) on kiln-sound:*master* (if on 1.0 0.0))
  (when (game-spatial g) (kiln-sound:spatial-volume (if on 1.0 0.0)))
  (when (game-sound-button g) (kiln-swift:ui-set-text (game-sound-button g) (if on "sound: on" "sound: off")))
  (dolist (i (game-sound-items g)) (kiln-mac:menu-check i on)))

(defun set-speed (g speed)
  (setf (game-speed g) (max 0.05 (min 0.8 speed)))
  (when (game-speed-slider g) (kiln-swift:ui-set-value (game-speed-slider g) (round (* 125 (game-speed g))))))

(defun set-head (g on)
  "Follow the head (AirPods) or not."
  (setf (game-head g) on)
  (format t "~&hello3d: following the head ~:[off~;on~]~%" on)
  (kiln-sound:spatial-head on)
  (when (game-head-button g) (kiln-swift:ui-symbol-button-set (game-head-button g) on))
  (dolist (i (game-head-items g)) (kiln-mac:menu-check i on)))

(defun toggle-head (g)
  "FOLLOW THE HEAD, off until asked for: where the system will ask for Motion & Fitness, the person is told first what
   it is for, and may say not now; refused before, where in Settings to allow it."
  (cond ((game-head g) (set-head g nil))
        ((eq (kiln-sound:spatial-head-permission) :not-asked)
         (let ((alert (kiln-swift:ui-alert
                       "Follow your head?"
                       "With AirPods, each sound stays in place as you turn your head. Next, you'll be asked to allow Motion & Fitness.")))
           (kiln-swift:ui-alert-action alert "Not Now")
           (kiln-swift:ui-alert-action alert "Continue")
           (kiln-swift:on-event :alert alert (lambda (index h) (declare (ignore h)) (when (eql index 1) (set-head g t))))
           (kiln-swift:ui-alert-present alert)))
        ((eq (kiln-sound:spatial-head-permission) :refused)
         (let ((alert (kiln-swift:ui-alert
                       "Motion & Fitness is off"
                       "To follow your head, allow Hello 3D in Settings > Privacy & Security > Motion & Fitness.")))
           (kiln-swift:ui-alert-action alert "OK")
           (kiln-swift:ui-alert-present alert)))
        (t (set-head g t))))

(defun make-hud (g)
  "The platform's own controls over the picture, inside the safe area (a phone's notch, turned) -- on a Mac on a pane
   of Liquid Glass, its buttons glass too."
  (let* ((scale (game-scale g))
         (x (+ 20 (floor (%sys 1001 4 0 0) scale))) (y (+ 8 (floor (max 0 (%sys 1001 3 0 0)) scale)))
         ;; the width there is, in points: a phone held upright has under 400, so lines are shorter and the
         ;; sound button goes under spin rather than beside it
         (w (- (floor (%sys 1001 0 0 0) scale) x 20 (floor (max 0 (%sys 1001 6 0 0)) scale)))
         (narrow (< w 480))
         (below (if narrow 40 0))
         ;; ON A MAC, A PANE OF GLASS: what goes on it is placed from its top left
         (panel (and (not (game-touch g)) (kiln-mac:glass-panel (- x 12) y 560 160 :radius 20)))
         (x (if panel 14 x))
         (y (if panel -2 y)))
    (flet ((put (h) (when panel (kiln-swift:ui-add panel h)) h)
           (glassy (h) (when panel (kiln-mac:glass-button h)) h))
      (put (kiln-swift:ui-label "Hello, world" x (+ y 8) (min w 300) 30))
      (setf (game-fps-label g) (put (kiln-swift:ui-label "" x (+ y 38) (min w 300) 22)))
      (put (kiln-swift:ui-label (cond ((not (game-touch g)) "click: look around   Esc: let go   WASD, QE: move   arrows: turn")
                                      (narrow "left thumb: move   right thumb: look")
                                      (t "left thumb: move   right thumb: look   or a game controller"))
                                x (+ y 62) (min w 600) 22))
      (setf (game-pad-label g) (put (kiln-swift:ui-label "" x (+ y 132 below) (min w 600) 22)))
      (let ((spin (put (glassy (kiln-swift:ui-button "spin: on" x (+ y 92) 110 32))))
            (speed (put (kiln-swift:ui-slider 50 (+ x 120) (+ y 96) 200 24)))     ; mid-way: 0.4
            (sound (put (glassy (if narrow
                                    (kiln-swift:ui-button "sound: on" x (+ y 92 below) 110 32)
                                    (kiln-swift:ui-button "sound: on" (+ x 340) (+ y 92) 110 32))))))
        (setf (game-spin-button g) spin (game-speed-slider g) speed (game-sound-button g) sound)
        (kiln-swift:on-tap spin (lambda (v h) (declare (ignore v h)) (set-spin g (not (game-spin g)))))
        (kiln-swift:on-tap sound (lambda (v h) (declare (ignore v h)) (set-sound g (not (game-sound g)))))
        (kiln-swift:on-event :slider speed (lambda (value h) (declare (ignore h)) (setf (game-speed g) (/ value 125.0)))))
      (when (game-spatial g)
        (let ((head (put (if narrow
                             (kiln-swift:ui-symbol-button "headphones" (+ x 120) (+ y 92 below) 48 32)
                             (kiln-swift:ui-symbol-button "headphones" (+ x 460) (+ y 92) 48 32)))))
          (setf (game-head-button g) head)
          (kiln-swift:on-tap head (lambda (v h) (declare (ignore v h)) (toggle-head g))))))))

(defun picture-texture (path &key (largest 512))
  "The image at PATH (a JPEG, PNG, WebP ...) as a texture, scaled down to at most LARGEST pixels across (nearest);
   NIL if it does not decode."
  (let* ((bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence b in) b)))
         (img (ignore-errors (pigment:decode-image-bytes bytes))))
    (when (and img (plusp (pigment:img-w img)) (plusp (pigment:img-h img)))
      (let* ((w (pigment:img-w img)) (h (pigment:img-h img)) (src (pigment:img-rgba img))
             (k (max 1 (ceiling (max w h) largest)))
             (tw (max 1 (floor w k))) (th (max 1 (floor h k)))
             (px (make-array (* tw th 4) :element-type '(unsigned-byte 8))))
        (dotimes (y th)
          (dotimes (x tw)
            (let ((s (* 4 (+ (* y k w) (* x k)))) (d (* 4 (+ (* y tw) x))))
              (setf (aref px d) (aref src s) (aref px (+ d 1)) (aref src (+ s 1))
                    (aref px (+ d 2)) (aref src (+ s 2)) (aref px (+ d 3)) 255))))
        (kiln-gpu:gpu-texture px tw th)))))

(defun on-picture (g path)
  "A file dropped on the window (or opened with the app): an image becomes the cubes' faces."
  (format t "~&hello3d: dropped ~A~%" path) (finish-output)
  (let ((tex (ignore-errors (picture-texture path))))
    (if tex
        (setf (game-crate-texture g) tex)
        (format t "~&hello3d: not a picture this can read~%"))))

(defun make-menus (g)
  "A Mac's menus: Game in the menu bar -- spin, sound, following the head, faster, slower -- and a status item with
   the frame rate, whose menu has spin and sound.  The check marks follow the toggles wherever they are changed."
  (let ((m (kiln-mac:menu "Game")))
    (push (kiln-mac:menu-item m "Spin" (lambda () (set-spin g (not (game-spin g)))) :key "s") (game-spin-items g))
    (push (kiln-mac:menu-item m "Sound" (lambda () (set-sound g (not (game-sound g))))) (game-sound-items g))
    (when (game-spatial g)
      (push (kiln-mac:menu-item m "Follow Your Head" (lambda () (toggle-head g))) (game-head-items g)))
    (kiln-mac:menu-separator m)
    (kiln-mac:menu-item m "Faster" (lambda () (set-speed g (* (game-speed g) 1.25))) :key "=")
    (kiln-mac:menu-item m "Slower" (lambda () (set-speed g (/ (game-speed g) 1.25))) :key "-"))
  (let ((sm (kiln-mac:menu "Hello 3D" :in-bar nil)))
    (push (kiln-mac:menu-item sm "Spin" (lambda () (set-spin g (not (game-spin g))))) (game-spin-items g))
    (push (kiln-mac:menu-item sm "Sound" (lambda () (set-sound g (not (game-sound g))))) (game-sound-items g))
    (setf (game-status g) (kiln-mac:status-item :symbol "cube.fill" :title "" :menu sm)))
  (set-spin g (game-spin g))
  (set-sound g (game-sound g))
  (dolist (i (game-head-items g)) (kiln-mac:menu-check i (game-head g)))
  ;; THE HUMS GO QUIET while the app is in the background, and come back with it (as sound is set, not forced on)
  (kiln-mac:on-app-state (lambda (state)
                           (let ((heard (and (game-sound g) (member state '(:active :shown)))))
                             (format t "~&hello3d: ~(~A~)~%" state) (finish-output)
                             (setf kiln-sound:*master* (if heard 1.0 0.0))
                             (when (game-spatial g) (kiln-sound:spatial-volume (if heard 1.0 0.0))))))
  (kiln-mac:on-quit (lambda () (format t "~&hello3d: quitting~%") (finish-output)))
  ;; A PICTURE dropped on the window, or opened with the app, becomes the cubes' faces
  (kiln-mac:on-drop (lambda (path) (on-picture g path)))
  (kiln-mac:on-open (lambda (path) (on-picture g path))))

(defun start (&key touch)
  "Everything made once: the view filling the window, the pipeline, the meshes, the textures, the controls."
  (let ((g (make-game :touch touch :scale (max 1 (%sys 1001 2 0 0)))))
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
    ;; SOUND: a hum a cube, looping and placed each frame (HEAR); a blip for a press
    ;; in 3D where the platform renders it (Spatial.swift), else panned in Lisp's own mix
    (when (kiln-sound:sound-open)
      (let ((tones (append (loop for f in *ring-notes* collect (kiln-sound:tone f))
                           (list (kiln-sound:tone 110 :timbre '(0.5 0.3 0.15 0.08))))))
        (setf (game-spatial g) (kiln-sound:spatial-open)
              (game-hums g) (if (game-spatial g)
                                (loop for tone in tones collect (kiln-sound:spatial-voice tone :loop t))
                                (loop for tone in tones collect (kiln-sound:play tone :gain 0.0 :loop t)))
              (game-blip g) (kiln-sound:pluck 880))
        (format t "~&hello3d: sound ~:[panned left and right~;in 3D (the platform's renderer)~]~%" (game-spatial g))))
    (make-hud g)
    (unless touch (make-menus g))
    (setf (game-fps-at g) (get-internal-real-time))
    g))

(defun main (&key touch)
  "Hello, world in 3D: start, then a frame each time the screen refreshes.  TOUCH: a phone's two thumbs."
  (%sys 1002 0 (+ (%sys 1001 0 0 0) (* (%sys 1001 1 0 0) 65536)) *dusk*)   ; under the view, until it draws
  (%sys 1003 0 0 0)
  (let ((g (start :touch touch)) (last (get-internal-real-time)))
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
        (handler-case (hear g)
          (serious-condition (c) (format t "~&hello3d: sound: ~A~%" c)))
        (handler-case (render g)
          (serious-condition (c) (format t "~&hello3d: ~A~%" c)))
        ;; the frame rate, twice a second
        (incf (game-frames g))
        (let ((since (/ (- now (game-fps-at g)) internal-time-units-per-second)))
          (when (>= since 0.5)
            (kiln-swift:ui-set-text (game-fps-label g)
                                    (format nil "~D frames a second" (round (game-frames g) since)))
            (when (game-status g)
              (kiln-mac:status-title (game-status g) (format nil " ~D fps" (round (game-frames g) since))))
            ;; the controller: said when one comes or goes, and its light the middle cube's colour
            (let ((n (kiln-pad:pad-count)))
              (unless (= n (game-pad-seen g))
                (setf (game-pad-seen g) n)
                (kiln-swift:ui-set-text (game-pad-label g)
                                        (if (plusp n)
                                            (format nil "controller: ~A~:[ -- sticks move and look, triggers lift, A spins~;~]"
                                                    (kiln-pad:pad-name 0) (game-touch g))
                                            "no controller (pair one in Bluetooth settings)"))
                (when (plusp n) (kiln-pad:pad-light 0 #xffd98c))))
            (setf (game-frames g) 0 (game-fps-at g) now)))))))
