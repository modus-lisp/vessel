;;;; gpu.lisp -- the GPU from Lisp, shaped like WebGPU: a device's buffers, shader modules and render pipelines,
;;;; and a frame recorded as a render pass -- set the pipeline, bind buffers, draw -- then submitted.  Nothing
;;;; draws until the pass ends; the GPU then runs it while Lisp goes on.
;;;;
;;;; One API, a backend per platform, under the same call names: Metal on the phone and the Mac (modus's
;;;; host/ios/swift/GPU.swift, through the Swift bridge), WebGPU in a browser (the web build's bridge, answered in
;;;; JavaScript).  Shaders are the one place they differ: Metal Shading Language here, WGSL there.
;;;;
;;;;   (gpu-view x y w h)                       a view the GPU draws; a UI handle (UI-REMOVE removes it)
;;;;   (gpu-shader "msl source")                a shader module
;;;;   (gpu-pipeline shader "vertex_fn" "fragment_fn")
;;;;   (gpu-buffer data)                        DATA: a single-float vector, an (unsigned-byte 8) vector, or a size
;;;;   (gpu-write buffer data &optional offset)
;;;;   (with-gpu-pass (pass view :clear #x101820) (gpu-use pass pipeline) (gpu-bind pass buffer 0) (gpu-draw pass 3))
;;;;
;;;; A single-float vector's payload is IEEE singles already, so vertex data and uniforms go to the GPU as they are.
;;;;
;;;; For 3D: a view has a depth buffer, tested and written by default (GPU-DEPTH changes that); faces are kept both
;;;; ways round unless GPU-CULL says (counter-clockwise is the front).
;;;;   (gpu-view 0 0 0 0)                       a view filling the window, following it as it resizes
;;;;   (gpu-view-size view)                     (values width height) of its drawable, in pixels
;;;;   (gpu-depth pass :test-and-write)         or :none, or :test (see-through things)
;;;;   (gpu-cull pass :back)                    or :none, :front
;;;;   (gpu-constants pass floats index)        a few KB of constants for the next draws (a model matrix): copied
;;;;   (gpu-texture rgba width height)          an RGBA8 texture, mipmapped; RGBA an (unsigned-byte 8) vector
;;;;   (gpu-bind-texture pass texture index)    and a linear, repeating sampler at the same index
;;;;   (gpu-indices list-or-vector)             an index buffer of 16-bit indices
;;;;   (gpu-draw-indexed pass indices count)

(defpackage #:kiln-gpu
  (:use #:cl)
  (:export #:gpu-view #:gpu-shader #:gpu-pipeline #:gpu-buffer #:gpu-write
           #:gpu-pass-begin #:gpu-pass-end #:with-gpu-pass #:gpu-use #:gpu-bind #:gpu-draw #:floats
           #:gpu-view-size #:gpu-depth #:gpu-cull #:gpu-constants #:gpu-texture #:gpu-bind-texture
           #:gpu-indices #:gpu-draw-indexed #:gpu-print #:gpu-view-hdr #:gpu-headroom))

(in-package #:kiln-gpu)

(defun %call (name &rest args) (apply #'kiln-swift:swift-call name args))

(defun %data (data)
  "DATA as (values address bytes): a single-float vector's lanes are 4 bytes, a byte vector's 1."
  (values (kiln-swift::%addr data)
          (if (eq (array-element-type data) 'single-float) (* 4 (length data)) (length data))))

(defun floats (&rest xs)
  "A single-float vector of XS: vertex data or uniforms."
  (let ((v (make-array (length xs) :element-type 'single-float)))
    (loop for x in xs for i from 0 do (setf (aref v i) (float x 1.0)))
    v))

(defun gpu-view (x y w h)
  (%call "gpu_view_new" x y w h))

;;; HIGH DYNAMIC RANGE: WebGPU's configure({format: 'rgba16float', toneMapping: {mode: 'extended'}}).  The view
;;; draws half floats in extended sRGB: what a shader wrote before looks the same, and past 1.0 is brighter than
;;; white, up to GPU-HEADROOM.  On a TV it also asks for an HDR10 mode (taken when Match Content > Dynamic Range
;;; is on).  Pipelines follow their view's format by themselves.

(defun gpu-view-hdr (view &optional (on t))
  "VIEW in extended range (ON) or back to 8 bits; true if it is HDR now, NIL where the host has no HDR."
  (handler-case (plusp (%call "gpu_view_hdr" view (if on 1 0)))
    (error () nil)))

(defun gpu-headroom ()
  "How many times SDR white the screen can show now: 1.0 on an SDR screen, or a TV not yet in an HDR mode."
  (handler-case (/ (%call "gpu_headroom") 1000.0)
    (error () 1.0)))

(defun gpu-shader (source)
  "A shader module compiled from SOURCE, or NIL (the log says why)."
  (let* ((b (kiln-swift::%octets source))
         (h (%call "gpu_shader_new" (kiln-swift::%addr b) (length b))))
    (and (plusp h) h)))

(defun gpu-pipeline (shader vertex-fn fragment-fn)
  "A render pipeline: SHADER's VERTEX-FN and FRAGMENT-FN, drawing over what is there (alpha blended)."
  (let* ((v (kiln-swift::%octets vertex-fn))
         (f (kiln-swift::%octets fragment-fn))
         (h (%call "gpu_pipeline_new" shader (kiln-swift::%addr v) (length v) (kiln-swift::%addr f) (length f))))
    (and (plusp h) h)))

(defun gpu-buffer (data)
  "A buffer holding a copy of DATA, or of SIZE zero bytes when DATA is an integer."
  (let ((h (if (integerp data)
               (%call "gpu_buffer_new" 0 data)
               (multiple-value-bind (a n) (%data data) (%call "gpu_buffer_new" a n)))))
    (and (plusp h) h)))

(defun gpu-write (buffer data &optional (offset 0))
  (multiple-value-bind (a n) (%data data)
    (%call "gpu_buffer_write" buffer offset a n)))

(defun gpu-pass-begin (view &key (clear 0))
  "This frame's render pass on VIEW, cleared to the colour CLEAR (#xRRGGBB); NIL when the view is gone."
  (let ((h (%call "gpu_pass_begin" view clear)))
    (and (plusp h) h)))

(defun gpu-pass-end (pass)
  "Submit PASS: the GPU draws it and the view shows it."
  (%call "gpu_pass_end" pass))

(defmacro with-gpu-pass ((pass view &key (clear 0)) &body body)
  "Record BODY into a render pass on VIEW and submit it.  NIL, doing nothing, when the view is gone."
  `(let ((,pass (gpu-pass-begin ,view :clear ,clear)))
     (when ,pass
       (unwind-protect (progn ,@body) (gpu-pass-end ,pass))
       t)))

(defun gpu-use (pass pipeline) (%call "gpu_set_pipeline" pass pipeline))

(defun gpu-bind (pass buffer index &key (stage :vertex))
  "BUFFER at INDEX for the vertex function, the fragment function, or :BOTH."
  (%call "gpu_set_buffer" pass buffer index (ecase stage (:vertex 0) (:fragment 1) (:both 2))))

(defun gpu-draw (pass count &key (start 0) (primitive :triangles))
  (%call "gpu_draw" pass (ecase primitive (:triangles 0) (:triangle-strip 1) (:lines 2) (:points 3)) start count))

;;; ---- 3D ------------------------------------------------------------------------------------------------------

(defun %stage (stage) (ecase stage (:vertex 0) (:fragment 1) (:both 2)))

(defun gpu-view-size (view)
  "VIEW's drawable size in pixels: (values width height), 0 0 before it is on the screen."
  (let ((s (%call "gpu_view_size" view)))
    (values (floor s 65536) (mod s 65536))))

(defun gpu-depth (pass mode)
  (%call "gpu_set_depth" pass (ecase mode (:none 0) (:test-and-write 1) (:test 2))))

(defun gpu-cull (pass mode)
  (%call "gpu_set_cull" pass (ecase mode (:none 0) (:back 1) (:front 2))))

(defun gpu-constants (pass data index &key (stage :both))
  "DATA (a single-float vector, at most 4 KB) as constants at INDEX for the draws that follow: copied now, so the
   next draw's may differ (a model matrix each)."
  (multiple-value-bind (a n) (%data data)
    (%call "gpu_set_bytes" pass a n index (%stage stage))))

(defun gpu-texture (rgba width height)
  "An RGBA8 texture of WIDTH x HEIGHT pixels from RGBA (an (unsigned-byte 8) vector, 4 bytes a pixel), with its
   smaller levels made; NIL if it cannot be."
  (assert (>= (length rgba) (* 4 width height)))
  (let ((h (%call "gpu_texture_new" (kiln-swift::%addr rgba) width height)))
    (and (plusp h) h)))

(defun gpu-bind-texture (pass texture index &key (stage :fragment))
  "TEXTURE at INDEX, and the sampler (linear, mipmapped, repeating) at the same index."
  (%call "gpu_set_texture" pass texture index (%stage stage)))

(defun gpu-indices (indices)
  "An index buffer of INDICES (a list or vector of integers below 65536), as 16-bit indices."
  (let* ((n (length indices))
         (b (make-array (* 2 n) :element-type '(unsigned-byte 8))))
    (let ((i 0))
      (map nil (lambda (x) (setf (aref b i) (ldb (byte 8 0) x) (aref b (1+ i)) (ldb (byte 8 8) x)) (incf i 2))
           indices))
    (gpu-buffer b)))

(defun gpu-draw-indexed (pass indices count &key (first 0) (primitive :triangles))
  "Draw COUNT of the 16-bit INDICES (a GPU-INDICES buffer) from FIRST."
  (%call "gpu_draw_indexed" pass (ecase primitive (:triangles 0) (:triangle-strip 1) (:lines 2) (:points 3))
         indices count first 0))

(defun gpu-print (view)
  "VIEW's next frame, copied out of the GPU, to the system's print panel."
  (%call "gpu_view_print" view))
