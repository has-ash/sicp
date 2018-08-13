#lang sicp

; preliminary stuff that we need

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


; join element with a set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set) ) )

; The union operation for unordered lists, given the adjoin, is then
; simply the adjoin of all elements in set2 with set 1

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set
       (adjoin-set (car set2) set1)
       (cdr set2) ) ) )

; examples

(define a (list 1 2 3) )
(define b (list 3 4 5) )

(display (union-set a b) )
