#lang sicp

; Similar structure to intersection
(define (union-set set_a set_b)
  (cond ( (null? set_a) set_b)
        ( (null? set_b) set_a)
        ( else 
        (let ( (head_a (car set_a) )
               (head_b (car set_b) ) )
          (cond ( (= head_a head_b)
                  (cons head_a
                          (union-set (cdr set_a) (cdr set_b) ) ) )
                ( (< head_a head_b)
                  (cons head_a
                          (union-set (cdr set_a) set_b) ) )
                ( else
                  (cons head_b
                          (union-set set_a (cdr set_b) ) ) ) ) ) ) ) )

; tests
(define A (list 1 5 9) )
(define B (list 0 2) )
(define C (list 1 3 4 10) )

(union-set A B)
(union-set B A)
(union-set A C)

(union-set A '() )
(union-set '() B)
