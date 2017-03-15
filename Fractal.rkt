#lang racket

;;;;;;;;;;;Gosper Curves;;;;;;;;;

(define (gosper-step curve)
  (define sfactor (/ 1 (sqrt 2)))
  (let* [(sc (scale sfactor sfactor curve))
         (c1 (rotate-around-origin (/ pi 4) sc))
         (c2 (rotate-around-origin (/ pi -4) sc))]
    (put-in-standard-position (connect-ends c1 c2))))

(define (gosper-curve level) 
  (lambda(curve)
    (if (= level 0) curve
        ((gosper-curve (- level 1)) (gosper-step curve)))))

(define (gosper-general curve calc-angle)
  (lambda (level)
    (if (= level 0) (put-in-standard-position curve)
        (let* [(sfactor (/ 1 (sqrt 2)))
               (st (put-in-standard-position curve))
               (sc (scale sfactor sfactor curve))
               (c1 (rotate-around-origin (calc-angle level) sc))
               (c2 (rotate-around-origin (- 0 (calc-angle level)) sc))
               (final (put-in-standard-position (connect-ends c1 c2)))]
          ((gosper-general final calc-angle) (- level 1))))))

;;;;;;;;;;;;;Koch Fractals;;;;;;;;;;;;;;;;;

(define (construct curve)
  (let* [(t curve)
         (c1 (rotate-around-origin (/ pi -2) t))
         (c2 (rotate-around-origin (/ pi 6) t))
         (t1 (connect-ends c1 c2))
         (c3 (rotate-around-origin (/ pi 1.2) t))
         (c (connect-ends t1 c3))]
    (translate 0 0 c)))

(define (koch-curve level)
  (translate -2 -1 (construct (khelp level 4))))

(define (khelp level size)
  (if (= level 0) (vertical-line (make-point 0 0) size)
      (let* [(c1 (khelp (- level 1) (/ size 3)))
             (c2 (rotate-around-origin (/ pi -3) c1))
             (c3 (rotate-around-origin (/ pi 3) c1))
             (t1 (connect-ends c1 c2))
             (t2 (connect-ends t1 c3))
             (c (connect-ends t2 c1))]
        c)))

;;;;;;;;;;;;;Sierpinski Triangles;;;;;;;;;;

(define (sierpinski level)
  (translate 0 2 (shelp level 4)))

(define (shelp level size)
  (define two 2)
  (if (= level 0) (rotate-around-origin (/ pi -1.5) (construct (vertical-line (make-point 0 0) size)))
      (construct2 (shelp (- level 1) (/ size two)) (/ size two))))

(define (construct2 curve size)
  (let* [(t curve)
         (ct (curve 0))
         (x0 (x-of ct))
         (y0 (y-of ct))
         (x- (- x0 (/ size 2)))
         (y- (- y0 (* 0.5 (* size (sqrt 3)))))
         (t- (translate (- x- x0) (- y- y0) t))
         (t1 (connect-rigidly t t- ))
         (x-- (+ x0 (/ size 2)))
         (y-- (- y0 (* 0.5 (* size (sqrt 3)))))
         (t-- (translate (- x-- x0) (- y-- y0) t))
         (t2 (connect-rigidly t-- t ))
         (final (connect-rigidly t1 t2))]
    (translate (- 0 x0) (- 0 y0) final)))

#|(define (construct2 curve size)
  (let* [(t curve)
         (ct (curve 0))
         (x0 (x-of ct))
         (y0 (y-of ct))
         (x- (- x0 (/ size 2)))
         (y- (- y0 (* 0.5 (* size (sqrt 3)))))
         (t- (translate (- x- x0) (- y- y0) t))
         (t1 (connect-rigidly t t- ))
         (x-- (+ x0 (/ size 2)))
         (y-- (- y0 (* 0.5 (* size (sqrt 3)))))
         (t-- (translate (- x-- x0) (- y-- y0) t))
         (final (connect-rigidly t1 t-- ))]
    (translate (- 0 x0) (- 0 y0) final)))

(define (construct2 curve size)
  (let* [(t curve)
         (ct (curve 0))
         (x (x-of ct))
         (y (y-of ct))
         (l (vertical-line (make-point x y) size))
         (r150 (rotate-around-origin (/ pi 1.2) l))
         ;(r150e (point-curve (r150 1)))
         (t1 (connect-ends t r150))
         (t2 (connect-ends t1 t))
         (r-90 (rotate-around-origin (/ pi -2) l))
         ;(r-90e (point-curve (r-90 1)))
         (t3 (connect-ends t2 r-90))
         (t4 (connect-ends t3 t))
         (r30 (rotate-around-origin (/ pi 6) l))
         ;(r30e (point-curve (r30 1)))
         (final (connect-ends t4 r30))]
    (translate (- 0 x) (- 0 y) final)))
|#

;;;;;;;;;;;Helper Assignment code;;;;;;;;;;

(require plot)
(plot-new-window? #t)
(plot-width 900)

(plot-height 900)
(define (draw curve)
  (plot (parametric
         (lambda (t) (vector (x-of (curve t))
                             (y-of (curve t))))
         0 1 #:width 1 #:samples 20000
         #:x-min -4 #:x-max 4
         #:y-min -4 #:y-max 4)))
; plot-width and plot-height give the size of the window
; #:width refers to the thickness of the line
; #:samples is the number of sample points. decrease it if
; your program takes too much time
; #:x-min -1 #:x-max 2 says that the graph is plotted in
; the x-axis range -1 to 2

;;;;;;;;;Question 2 code;;;;;;;;;

;;;;Translate
(define (translate x y curve)
  (lambda(t)
    (let ((ct (curve t)))
      (make-point (+ (x-of ct) x) (+ (y-of ct) y)))))

;;;;;Scale
(define (scale x y curve)
  (lambda(t)
    (let ((ct (curve t)))
      (make-point (* (x-of ct) x) (* (y-of ct) y)))))

;;;;;;Rotate
(define (rotate-around-origin radians curve)
  (lambda(t)
    (let* [(ct (curve t))
          (x (x-of ct))
          (y (y-of ct))
          (c (cos radians))
          (s (sin radians))
          (newx (- (* x c) (* y s)))
          (newy (+ (* x s) (* y c)))]
      (make-point newx newy))))

(define (put-in-standard-position curve)
    (let* [(c0 (curve 0))
           (x0 (x-of c0))
           (y0 (y-of c0))
           (tc (translate (- 0 x0) (- 0 y0) curve))
           (c1 (tc 1))
           (x1 (x-of c1))
           (y1 (y-of c1))
           (rc (rotate-around-origin (angle x1 y1) tc))
           (c2 (rc 1))
           (x2 (x-of c2))
           (y2 (y-of c2))]
      (scale (/ 1 x2) (/ 1 x2) rc)))

(define (connect-ends curve1 curve2)
    (let* [(c1 (curve1 1))
           (x (x-of c1))
           (y (y-of c1))
           (c2 (curve2 0))
           (x2 (x-of c2))
           (y2 (y-of c2))
           (newcurve2 (translate (- x x2) (- y y2) curve2))]
      (connect-rigidly curve1 newcurve2)))

;;;;;;;;;;;My Helper code;;;;;;;;;;

(define (angle x y)
  (cond
    ((> x 0) -(atan (/ y x)))
    ((< x 0) (- pi (atan (/ y x))))
    ((= x 0) (if (= y 0) 0 (if (> y 0) (/ pi 2) (/ pi -2))))))

;;;;;;;;;;;Helper Assignment code;;;;;;;;;;

;;;Vertical Line
(define (vertical-line p l)
  (lambda(t) (make-point (x-of p) (+ (y-of p) (* l t)))))

(define (connect-rigidly curve1 curve2)
(lambda (t)
(if (< t 0.50000000)
(curve1 (* 2 t))
(curve2 (- (* 2 t) 1)))))

(define (make-point x y)
(lambda (bit)
(if (zero? bit) x y)))

(define (x-of point)
(point 0))

(define (y-of point)
(point 1))

(define (unit-circle)
(lambda (t)
(make-point (sin (* 2 pi t))
(cos (* 2 pi t)))))

(define (unit-line-at y)
(lambda (t) (make-point t y)))

(define (point-curve p)
  (lambda (t) p))

(define unit-line (unit-line-at 0))