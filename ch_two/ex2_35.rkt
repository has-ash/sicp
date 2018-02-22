#lang sicp

;ex 2_35

; This is so beautiful.

; The authors ask us to count the leaves of a tree
; using accumulate. Now one might imagine that in order 
; to do this, we should first convert the tree into 
; a sequence (flatten the list) and then just go through 
; the sequence and add all the values. 

; However, there is a more elegant solution hiding in the corner.
; I define a procedure how-many-am-i? that returns 1 for elements
; at the top level of the list and otherwise calls count-leaves recursively
; to count the leaves of the sub tree. 


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )

(define (how-many-am-i? tree)
  (cond ( (not (pair? tree) ) 1 )
        (else (count-leaves tree) ) ) )

(define (count-leaves t)
  (accumulate +
              0
              (map how-many-am-i? t ) ) )

(define some-tree (list 1 (list 2 3) (list 4) 5 (list 6 (list 7 (list 11) 8) 9) 10 ) )

(map how-many-am-i? some-tree)

(count-leaves some-tree)
