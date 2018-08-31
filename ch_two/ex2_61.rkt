#lang sicp

; ex2.61

; define adjoin by analogy with element of set for ordered sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; adjoin-set is a recursive procedure just like element-of-set above.  

(define (adjoin-set set value)
  (if (null? set)
      (list value)
        (let (( head (car set) ) )
          (cond ( (= head value) set)
                ; append expects lists
                ( (< head value) (append (list head)
                                         (adjoin-set (cdr set) value) ) )
                ; else head > value
                (else (append (list value) set) ) ) ) ) )

;tests

(define A (list 1 2 3) )
(define B (list 1 9 20 ) )

(adjoin-set A 5)
(adjoin-set A 0)
(adjoin-set B 5)
(adjoin-set '() 4)


