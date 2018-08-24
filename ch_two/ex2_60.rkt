#lang sicp

; ex 2.60

; Since we can now have duplicates, when creating the adjoin, we can
; just add the element without seeing if the element already exists
; in our set. We can do the same thing for union-set by appending. This would drop
; the time complexity for adjoining to O(1) and for union to O(n) where
; the second set has n elements.

(define (adjoin-set x set)
  (cons x set) )

(define (union-set set1 set2)
  (append set1 set2) )

; However, we cannot do the same for element-of-set? since we need to check
; all the elements to be sure that the element is not in the set. So that stays the
; same as in 2.59.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Our intersection function has to rely on element of set too. So unfortunately,
; we'll have to go without changing it. 
