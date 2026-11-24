;;;; location.lisp -- where the device is, from Lisp, on the Mac and the phone.  The Swift side is modus's
;;;; host/ios/swift/Location.swift (location_*).  Ask (saying first what for), start, then read the latest fix.
;;;;
;;;;   (location-permission)            :not-asked :allowed :refused
;;;;   (location-ask fn)                the system's question (once); FN gets T or NIL as answered
;;;;   (location-start)  (location-stop)
;;;;   (location)                       the latest fix: (values latitude longitude accuracy altitude), or NIL
;;;;   (sun-position latitude longitude &optional universal-time)
;;;;                                    where the sun is in that sky: (values elevation azimuth), degrees -- above
;;;;                                    the horizon, and clockwise from north

(defpackage #:kiln-location
  (:use #:cl)
  (:export #:location-permission #:location-ask #:location-start #:location-stop #:location #:sun-position))

(in-package #:kiln-location)

(defun %call (name &rest args) (apply #'kiln-swift:swift-call name args))

(defun location-permission ()
  (case (%call "location_permission") (0 :not-asked) (1 :allowed) (t :refused)))

(defun location-ask (fn)
  "Ask the system for the device's location; FN gets T if allowed, NIL if not."
  (kiln-swift:on-event :locationask 0 (lambda (ok h) (declare (ignore h)) (funcall fn (eql ok 1))))
  (%call "location_ask"))

(defun location-start () (%call "location_start"))
(defun location-stop () (%call "location_stop"))

(defvar *fix* (make-array 4 :element-type 'single-float :initial-element 0.0))

(defun location ()
  "The latest fix: (values latitude longitude accuracy altitude) -- degrees, then metres -- or NIL if none yet."
  (when (eql 1 (%call "location_get" (kiln-swift::%addr *fix*)))
    (values (aref *fix* 0) (aref *fix* 1) (aref *fix* 2) (aref *fix* 3))))

(defun sun-position (latitude longitude &optional (universal-time (get-universal-time)))
  "Where the sun is seen from LATITUDE LONGITUDE (degrees, east positive) at UNIVERSAL-TIME: (values elevation
   azimuth) in degrees, elevation above the horizon and azimuth clockwise from north.  The astronomers' low-precision
   formulas: good to a fraction of a degree, plenty for a light."
  (flet ((rad (d) (* d (/ pi 180))) (deg (r) (* r (/ 180 pi))))
    (let* ((d (- (+ (/ (- universal-time 2208988800) 86400d0) 2440587.5d0) 2451545d0))  ; days from J2000
           (g (rad (+ 357.529d0 (* 0.98560028d0 d))))
           (q (+ 280.459d0 (* 0.98564736d0 d)))
           (l (rad (+ q (* 1.915d0 (sin g)) (* 0.020d0 (sin (* 2 g))))))
           (e (rad (- 23.439d0 (* 0.00000036d0 d))))
           (ra (atan (* (cos e) (sin l)) (cos l)))
           (dec (asin (* (sin e) (sin l))))
           (gmst (mod (+ 18.697374558d0 (* 24.06570982441908d0 d)) 24))
           (ha (- (rad (+ (* gmst 15) longitude)) ra))
           (lat (rad latitude))
           (el (asin (+ (* (sin lat) (sin dec)) (* (cos lat) (cos dec) (cos ha)))))
           (az (atan (- (sin ha)) (- (* (tan dec) (cos lat)) (* (sin lat) (cos ha))))))
      (values (deg el) (mod (deg az) 360)))))
