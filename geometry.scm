;; geometry.scm
;; Geometric primitives for the New Babylon DSL

(define-module (new-babylon geometry)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib cell)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-point
            make-polygon
            make-box
            make-cylinder
            point-distance
            contains-point?
            intersect?
            ^point
            ^polygon
            ^box
            ^cylinder))

;; ==========================================
;; Record types for geometric calculations
;; ==========================================

;; Point in 3D space
(define-record-type <point>
  (make-point-record x y z)
  point-record?
  (x point-x)
  (y point-y)
  (z point-z))

;; Polygon in 3D space (defined by points and height)
(define-record-type <polygon>
  (make-polygon-record points height)
  polygon-record?
  (points polygon-points)
  (height polygon-height))

;; Box in 3D space
(define-record-type <box>
  (make-box-record origin dimensions)
  box-record?
  (origin box-origin)
  (dimensions box-dimensions))

;; Cylinder in 3D space
(define-record-type <cylinder>
  (make-cylinder-record center radius height)
  cylinder-record?
  (center cylinder-center)
  (radius cylinder-radius)
  (height cylinder-height))

;; ==========================================
;; Helper functions for geometric calculations
;; ==========================================

;; Calculate distance between two points
(define (point-distance p1 p2)
  (let ((dx (- (point-x p2) (point-x p1)))
        (dy (- (point-y p2) (point-y p1)))
        (dz (- (point-z p2) (point-z p1))))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

;; Check if a polygon contains a point (simplified)
(define (contains-point? polygon point)
  ;; This is a simplified placeholder for point-in-polygon algorithm
  ;; Should implement proper algorithm based on ray casting or similar
  #t)

;; Check if two geometry objects intersect (simplified)
(define (intersect? geom1 geom2)
  ;; This is a simplified placeholder for intersection detection
  ;; Should implement proper intersection algorithm based on geometry types
  #f)

;; ==========================================
;; Goblins actor constructors for geometric objects
;; ==========================================

;; Point constructor
(define (^point bcom x y z)
  (let ((point-record (make-point-record x y z)))
    (methods
     ((coordinates) (list x y z))
     ((x) x)
     ((y) y)
     ((z) z)
     ((distance other-point)
      (let-on ((ox (<- other-point 'x))
               (oy (<- other-point 'y))
               (oz (<- other-point 'z)))
        (let ((dx (- ox x))
              (dy (- oy y))
              (dz (- oz z)))
          (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))))
     ((translate dx dy dz)
      (spawn ^point bcom (+ x dx) (+ y dy) (+ z dz)))
     ((data) point-record))))

;; Polygon constructor
(define (^polygon bcom points height)
  (let ((polygon-record (make-polygon-record points height)))
    (methods
     ((points) points)
     ((height) height)
     ((contains? point)
      ;; Simplified method - should use proper point-in-polygon test
      #t)
     ((area)
      ;; Calculate area of polygon (simplified)
      (* 10 (length points)))
     ((centroid)
      ;; Calculate centroid of points (simplified)
      (let ((sum-x 0)
            (sum-y 0)
            (sum-z 0)
            (n (length points)))
        (for-each
         (lambda (p)
           (set! sum-x (+ sum-x (car p)))
           (set! sum-y (+ sum-y (cadr p)))
           (set! sum-z (+ sum-z (caddr p))))
         points)
        (list (/ sum-x n) (/ sum-y n) (/ sum-z n))))
     ((data) polygon-record))))

;; Box constructor
(define (^box bcom origin dimensions)
  (let ((box-record (make-box-record origin dimensions)))
    (methods
     ((origin) origin)
     ((dimensions) dimensions)
     ((volume)
      (let ((w (car dimensions))
            (h (cadr dimensions))
            (d (caddr dimensions)))
        (* w h d)))
     ((contains? point)
      (let ((ox (car origin))
            (oy (cadr origin))
            (oz (caddr origin))
            (px (car point))
            (py (cadr point))
            (pz (caddr point))
            (w (car dimensions))
            (h (cadr dimensions))
            (d (caddr dimensions)))
        (and (>= px ox)
             (< px (+ ox w))
             (>= py oy)
             (< py (+ oy h))
             (>= pz oz)
             (< pz (+ oz d)))))
     ((data) box-record))))

;; Cylinder constructor
(define (^cylinder bcom center radius height)
  (let ((cylinder-record (make-cylinder-record center radius height)))
    (methods
     ((center) center)
     ((radius) radius)
     ((height) height)
     ((volume)
      (* 3.14159 (* radius radius) height))
     ((contains? point)
      (let* ((cx (car center))
             (cy (cadr center))
             (cz (caddr center))
             (px (car point))
             (py (cadr point))
             (pz (caddr point))
             (dx (- px cx))
             (dy (- py cy))
             (horiz-dist (sqrt (+ (* dx dx) (* dy dy)))))
        (and (<= horiz-dist radius)
             (>= pz cz)
             (<= pz (+ cz height)))))
     ((data) cylinder-record))))

;; ==========================================
;; Public API functions
;; ==========================================

;; Create a new point
(define (make-point x y z)
  (spawn ^point (spawn-bcom) x y z))

;; Create a new polygon
(define (make-polygon points height)
  (spawn ^polygon (spawn-bcom) points height))

;; Create a new box
(define (make-box origin dimensions)
  (spawn ^box (spawn-bcom) origin dimensions))

;; Create a new cylinder
(define (make-cylinder center radius height)
  (spawn ^cylinder (spawn-bcom) center radius height)) 