#lang sicp

; ex 2_32
; How does this work?
; An algorithm for constructing the power set
; would go as follows:
;
; A = {a, b, c}
;
; 1. P(A) = { {} }                                    ======> base case
; 2. for x in A:                                      ======> This is handled by the recursion
; 3.     temp_set = {}
; 4.     for y in P(A):
; 5.          temp_set = temp_set union { x union y } ======> this loop is the lambda function
; 6.     P(A) = P(A) union temp_set                   ======> this is the (append rest (map .... ) ) ) part
;
; Recursively we can state that
; the power set of a set A is the union of the power set of A with
; one element x removed, P(A-x), and the union of each set in P(A-x) with x.
;
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (m) (cons (car s) m) )
                         rest) ) ) ) )

(define a (list 1  2 3) )
(subsets a)