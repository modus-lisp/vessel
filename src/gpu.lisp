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

(defpackage #:kiln-gpu
  (:use #:cl)
  (:export #:gpu-view #:gpu-shader #:gpu-pipeline #:gpu-buffer #:gpu-write
           #:gpu-pass-begin #:gpu-pass-end #:with-gpu-pass #:gpu-use #:gpu-bind #:gpu-draw #:floats))

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
