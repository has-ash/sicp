#lang sicp

; Church Numerals as in exercise
(define zero (lambda (f) ( lambda (x) x ) ) )

(define (add-1 n)
  (lambda (f) ( lambda (x) ( f ( ( n f ) x ) ) ) ) )


; If you do use the substitution model for (add-1 zero) you will eventually get to the definition for one
; But even if you don't, the Church Numeral for one applies f to x once (for zero it applies f to x zero times
; and a similar concept holds for other numbers)

(define one
  (lambda (f)
              ( lambda (x) ( f x) ) ) )

; we can check this by defining the successor function
(define (successor x)
  (+ x 1) )

; This should return one
(display "TEST: numeral for one" )
(newline)

( (one successor) 0 )

; Similarly for two, we apply f to x two times
(define two
  (lambda (f)
    (lambda (x) (f (f x) ) ) ) )

; Let's check this too
(display "TEST: numeral for two")
(newline)

( (two successor) 0 )

; Now let's turn out attention to addition. Let's start by defining
; a function that applies a function m times to an argument

(define  (apply_times function x m)
  ( if (= m 0)
       x
       ( apply_times function (function x) (- m 1) ) ) )

; Let's test that this actually works
(display "TEST: increment zero, five times" )
(newline)

(apply_times successor 0 5)

; This seems fine. Now let's try to build a generic function for generating a church numeral

(define (church_numeral m)
  (lambda (f) (lambda (x) (apply_times f x m) ) ) )

; Let's test this as well
(display "TEST: create a church numeral" )
(newline)

( ( (church_numeral 5) successor) 0)

; We're almost done now.
; Here's how you should think about this:
; Look at how a church numeral works. The Church numeral for
; n, applies the function f to x n times. So we take one of the
; numerals and provide it with f and x. This returns  a function
; which we then pass on to the other numeral. 
(define (addition numeral_one numeral_two)
  ( lambda(f) (lambda (x) ( (numeral_one f) ( (numeral_two f) x) ) ) ) )

; Let's try this
(display "TEST: addition function 1+2")
(newline)

( ( (addition one two) successor ) 0)

; Let's create church numerals for 11 and 27 and add them
(define eleven (church_numeral 11) )

(define twenty-seven (church_numeral 27) )

(display "TEST: create and add 11 and 27" )
(newline)

( ( (addition eleven twenty-seven) successor ) 0 )
