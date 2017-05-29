#lang sicp
; cons is a function that returns a function which takes a function as an argument
; This is just beautiful

(define (cons x y)
(lambda (m) (m x y) ) )


; car takes a cons object z as argument
; Now remember, cons actually gives us a function (lambda)
; which takes a function as an argument. In this case, that
; function is defined inside car. Let's give them names so
; that we don't confuse ourselves. Let's call the function
; returned by cons to be ret_cons and it's argument to be
; ret_cons_arg.

; So basically car looks like this (written in a procedural language):
; z = ret_cons (ret_cons_arg)
; where ret_cons_arg is defined inside car and is
; ret_cons_arg = lambda (p q) and returns p#
(define (car z)
  (z (lambda (p q) p ) ) )

; So let's now use the substitution model:
; ( car z )
; ( z (lambda (p q) p )
; ( (lambda (m) (m x y) ) (lambda (p q) p ) )
; Now m is the argument of the function up there.
; (I'm writing the full definition of the function)
; ( (lambda (p q) p) (x y) )
; Which obviously gives back (x)

(define (cdr z)
  (z (lambda (x y) y) ) )

; EXAMPLES

(define z (cons 5 6) )

(car z)
(cdr z)