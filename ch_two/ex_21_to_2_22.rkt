#lang sicp

(define square (lambda (x) (* x x ) ) )  

;ex 2_21

(define (square-list items)
    (if (null? items)
        nil
        (cons (square (car items) ) (square-list (cdr items) ) ) ) )

(define (square-list-map items)
  (map square items) )

(square-list (list 1 2 3 4 5) ) 
(square-list-map (list 1 2 3 4 5 ) )

;ex 2_22
; 1. Louis Reasoner needs some magic pills because he/she is
;    reversing stuff in the cons part.
; 2. First cons call is something along the lines of
;    (cons nil (square (car things ) ) )
;    which is obviously a problem since the first element
;    is now nil. Try appending.

(define (square-list-iter-rev items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things) )
                    answer ) ) ) )
  (iter items nil) )

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things) ) ) ) ) )
    (iter items nil) )

(define evens (list 2 4 6 8 10) )
(square-list-iter-rev evens)
(square-list-iter evens) 

(define (square-list-with-some-sense items)
  (define (iter things answer)
    ( if (null? things)
         answer
         (iter (cdr things)
               (append answer
                       (list (square (car things) ) ) ) ) ) )
  (iter items nil) )

(square-list-with-some-sense evens)
(square-list-with-some-sense nil)
