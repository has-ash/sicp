#lang sicp

;constructor for line segment

(define (make-segment point_a point_b)
  (cons point_a point_b) )

;selector for line segment

(define (start-segment line_segment)
  (car line_segment) )

(define (end-segment line_segment)
  (cdr line_segment))

;constructor for points

(define (make-point x y)
  (cons x y))

;selectors for points

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;midpoint
(define (average x y)
  (/ (+ x y) 2.0))

(define (midpoint-segment line_segment)
  (let ( ( x1 (x-point (start-segment line_segment)))
         (x2 (x-point (end-segment line_segment)))
         (y1 (y-point (start-segment line_segment)))
         (y2 (y-point (end-segment line_segment))))
    (make-point (average x1 x2) (average y1 y2) ) ) )

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display " , ")
  (display (y-point point))
  (display ")") )

;Example usage
(print-point (midpoint-segment (make-segment
                                (make-point 1 1)
                                (make-point 4 4) )))
(print-point (midpoint-segment (make-segment
                                (make-point -1 -1)
                                (make-point 2 3) ) ))
