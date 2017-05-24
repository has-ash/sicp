#lang sicp
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b) ) ) )

(define (abs a)
  (if (< a 0)
      (- a)
      a))

(define (xor a b)
  (or (and (not a) b) (and a (not b) ) ) )

(define (positive? a)
  (> a 0) )

(define (make-rat num denom)
  (let ( ( divisor (abs (gcd num denom) ) ) )
    (if (xor (positive? num) (positive? denom) )
        (cons (/ (- (abs num) ) divisor) (/ (abs denom) divisor) )
               (cons (/ (abs num) divisor) (/ (abs denom) divisor) ) ) ) )         

(define (numerator rational) (car rational) )
(define (denominator rational) (cdr rational))

(define (print-rat rational)
  (newline)
  (display (numerator rational) )
  (display "/")
  (display (denominator rational) ) )

(print-rat (make-rat -2 -4))