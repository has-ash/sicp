#lang sicp
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x ) (upper-bound y)) ) )

( define (mul-interval x y)
  (let ((p1 (* (lower-bound x ) (lower-bound y) ))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x ) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y ) ) ) )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4) ) ) )

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

(define (make-center-width c w) 
  (make-interval (- c w) (+ c w) ) )

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i) ) 2 ) )

(define (width i)
  (/ (- (upper-bound i) (lower-bound i) ) 2 ) )

; 2_12
(define (make-center-percent center percent)
  (let ( (width (/ (* center percent) 100 ) ) )

    (make-center-width center width) ) )

(define (percent interval)
  (let ( (c (center interval) )
         (w (width interval) ) )
    (* (/ w c) 100 ) ) ) 

;test
(define interval (make-center-percent 10 6.5) )
(percent interval)

;2_13
; assume percent to be epsilon 
; then try to add the tolerance of the two intervals
; compare with the tolerance of the new
; multiply the old tolerances to get an expression you can
; substitute to disocver the relation

; Lem E Tweakit tweaks it

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2) ) )

(define (par2 r1 r2)
  (let ( (one (make-interval 1 1 ) ) )
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2) ) ) ) )

; Ex 2_14
; demonstrate different results

(define r1 (make-center-width 8 0.5) )
(define r2 (make-center-width 20 0.3) )

(par1 r1 r2)
(par2 r1 r2)

;test some intervals

(define A (make-center-percent 10 0.1) )
(define B (make-center-percent 10 0.3) )

(define (center-percent-test interval-one interval-two)
  (let ( (div-res (div-interval interval-one interval-two) ) )

    (display (center div-res) )
    (newline)

    (display ( percent div-res) )
    (newline) )  )

(center-percent-test A A)
(center-percent-test A B)
(center-percent-test B B)

; 2_15
; Eva's answer seems to be true since multiplying
; dividing and adding positive intervals all seem
; increase the uncertainty in our computation. If we
; can limit the numbers of operations that we perform
; on the intervals then intuitively, we should expect
; better error bounds. Of course, this does not take into
; account the degree of the uncertainty introduced by the
; different operations on intervals.

; 2_16
; It is a well known fact that the usual laws of arithmetic
; do not apply to floating point numbers.
; We always have to take rounding errors
; into account. There are other issues with the
; numerical stability of algorithms too.
;
; For example, take the distributive law:

(define arg1 (make-interval 80.4545 200.56098035) )
(define arg2 (make-interval 20.78978 30.345345) )
(define arg3 (make-interval 0.00132123467 0.00524545) )

(newline)
(display "Associative Law" )
(newline)

(define res1 (add-interval arg2 arg3) )
(define res2 (mul-interval arg3 res1) )

(define rev-res1 (mul-interval arg3 arg1) )
(define rev-res2 (mul-interval arg3 arg2) )
(define rev-res3 (add-interval rev-res1 rev-res2) ) 

res2
rev-res2
