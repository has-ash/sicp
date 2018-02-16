#lang sicp
; ex 2_23 - for-each

; give empty list - get empty list back 
(define (for-each-element procedure a-list)
  (cond ( (null? a-list) nil )
        (else
         (procedure (car a-list) )
         (for-each procedure (cdr a-list) )  ) ) ) 

(define (print-square x)
  (newline)
  (display (* x x)  ) )

(print-square 5)

(for-each-element print-square (list 1 2 3 4 5) )
(newline) 
(for-each-element print-square '() )