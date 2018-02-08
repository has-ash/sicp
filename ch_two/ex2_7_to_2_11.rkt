#lang sicp

; author : Hasan Ashraf
;exercise 2-7

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x ) (upper-bound y)) ) )

;ex 2.9
; Define sub-interval using the standard definition in interval arithmetic
; x_max - y_min > x_min - y_max

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y) )
                 (- (upper-bound x ) (lower-bound y) ) ) )

( define (mul-interval x y)
  (let ((p1 (* (lower-bound x ) (lower-bound y) ))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x ) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y ) ) ) )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4) ) ) )

; 2.9 counter_example

(define a (make-interval 0 1) )
(define b (make-interval 1 2) )
(define c (make-interval 2 3) )

; this will be different ( a, b, c have some width but their multiplication doesn't)
(mul-interval a b)
(mul-interval b c)

;exericse 2.10
;check if lower bound is neg and upper bound is positive

(define (div-interval x y)
  (if (and (negative? (lower-bound y) )
           (positive? (upper-bound y) ) )

      ;then
      (display "Divisor contains 0")
      ;otherwise
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y) )
                      (/ 1.0 (lower-bound y) ) ) ) ) )

;sanity check

(define x (make-interval 4 5) )
(define y (make-interval -1 1) )
(div-interval x y)

; 2.11 Ben Bitdiddle
; Make table of combinations
; some of them will be illegal ( x2 < x1 )
; eventually you'll be left with nine possibilities
; which can actually be paired up but I don't know how
; to express that succinctly. So, we'll do it the dumb way.

; This is the worst piece of code I've ever written.

(define (mul-interval-ben first second )
  (let ( (x1 (lower-bound first) )
         (x2 (upper-bound first) )
         (y1 (lower-bound second) )
         (y2 (upper-bound second) ) )
    
  (cond (
; 1
         (and
           (>= x1 0)
           (>= x2 0)
           (>= y1 0)
           (>= y2 0) )

          (make-interval (* x1 y1) (* x2 y2) ) )
;2
        ( (and
           (<= x1 0)
           (>= x2 0)
           (>= y1 0)
           (>= y2 0) )

          (make-interval (* y2 x1) (* x2 y2) ) )
;3
        ( (and
           (<= x1 0)
           (<= x2 0)
           (>= y1 0)
           (>= y2 0) )

          (make-interval (* x1 y2) (* x2 y1) ) )
;4
        ( (and
           (>= x1 0)
           (>= x2 0)
           (<= y1 0)
           (>= y2 0) )

          (make-interval (* x2 y1) (* x2 y2) ) )

;5
        ( (and
           (<= x1 0)
           (<= x2 0)
           (<= y1 0)
           (>= y2 0) )

          (make-interval (* x1 y2) (* x1 y1) ) )
;6
        ( (and
           (>= x1 0)
           (>= x2 0)
           (<= y1 0)
           (<= y2 0) )

          (make-interval (* y1 x2) (* y2 x1) ) )

;7
        ( (and
           (<= x1 0)
           (>= x2 0)
           (<= y1 0)
           (<= y2 0) )

          (make-interval (* y1 x2) (* x1 y1) ) )

;8
        ( (and
           (<= x1 0)
           (<= x2 0)
           (<= y1 0)
           (<= y2 0) )

          (make-interval (* x2 y2) (* x1 y1) ) )

;9
        ( (and
           (<= x1 0)
           (>= x2 0)
           (<= y1 0)
           (>= y2 0) )

          (make-interval (min (* x1 y2) (* x2 y1) )
                         (max (* x2 y2) (* x1 y1) ) ) ) ) ) )




; test by calling both mul-interval-ben and mul-interval

(define (intervals-equal? first second)
  (and (= (lower-bound first)
                (lower-bound second) )

             (= (upper-bound first)
                (upper-bound second) ) ) )


(define (test first second)
  ( let ( ( ben-res (mul-interval-ben first second) )
           (normal-res (mul-interval first second) ) )

    (if (intervals-equal? ben-res normal-res) 
        (display "TEST PASSED") 
        (display "TEST FAILED") ) 
     (newline) ) )

(define pos (make-interval 4 5) )
(define straddle (make-interval -2 3) )
(define neg (make-interval -10 -1) )

(newline)

;test all possibilities for the three intervals above
(test pos pos)
(test neg neg)
(test straddle straddle)

(test pos neg)
(test neg pos)

(test pos straddle)
(test straddle pos)

(test neg straddle)
(test straddle neg)
