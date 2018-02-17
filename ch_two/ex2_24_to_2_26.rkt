#lang sicp
;ex 2_24

(define a (list 1 ( list 2 ( list 3 4 ) ) ) )
;

; ex2_25

(define b (list 1 3 (list 5 7) 9) )

(define c (list 1 (list 2 (list 3 (list 4 (list 5 ( list 6 7 ) ) ) ) ) ) )

; A list within a list incurs an additional cons - a pair that has as its first element
; a reference to the list and the second as nil.
(car (cdr (car ( cdr ( cdr b )  ) ) ) )

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c) ) ) ) ) ) ) ) ) ) ) )

;ex 2_26

; Let's try to predict and then check

 (define x (list 1 2 3) )
 (define y (list 4 5 6) ) 
; (append x y) -> (mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '() ) ) ) ) ) )
(append x y)

; (cons x y) - > (mcons (mcons 1 (mcons 2 (mcons 3 '() ) ) ) (mcons 4 (mcons 5 (mcons 6 '() ) ) ) )
(cons x y)

; (mcons (mcons 1 (mcons 2 (mcons 3 '() ) ) ) (mcons (mcons 4 (mcons 5 (mcons 6 '() ) ) ) '() ) )
(list x y)

; That worked out fine!



