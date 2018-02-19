#lang sicp

; ex 2_28
; this follows the scheme
; of the count-leaves method
; introduced in the section

(define (fringe x)
  (cond ( (null? x)
          x)
        ( (not (pair? x) )
          (list x) )
        ( else (append
                (fringe (car x) )
                (fringe (cdr x) ) ) ) ) ) 

(define a (list (list 1 2) (list 3 4 ) ) )
(fringe a)
(fringe (list a a) ) 
