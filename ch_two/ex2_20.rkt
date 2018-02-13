#lang sicp
; ex 2_20
; let's implement our own filter function
; input: x is a list
; predicate is a function that returns a boolean for an element of x
(define (construct-list x predicate)
  (cond ( (null? x) x )
        ( (predicate (car x) ) (cons (car x) (construct-list (cdr x) predicate) ) )
        ( else (construct-list (cdr x) predicate) ) ) )

; This is just a wrapper around this general func now
(define (same-parity . x)
  (cond ( (null? x) x)
        ( (even? (car x) ) (construct-list x even?) )
        ( (odd? (car x) ) (construct-list x odd? ) )
        ( else (display "Integers, integers, my good man" ) ) ) ) 

       
(define squares (list 1 4 9 16 25 36) )
(construct-list squares even?)
(construct-list squares odd? )

(same-parity 1 2 3 4 5 6)
(same-parity 8 10 7 11 44)