#lang sicp
(#%require "./ex2_63.rkt")
(#%require "./ex2_64.rkt")

; Standing on the shoulders of giants
; i.e. using the O(n) solution to 2.62
(define (union-set set_a set_b)
  (cond ( (null? set_a) set_b)
        ( (null? set_b) set_a)
        ( else 
        (let ( (head_a (car set_a) )
               (head_b (car set_b) ) )
          (cond ( (eq? head_a head_b)
                  (cons head_a
                          (union-set (cdr set_a) (cdr set_b) ) ) )
                ( (< head_a head_b)
                  (cons head_a
                          (union-set (cdr set_a) set_b) ) )
                ( else
                  (cons head_b
                          (union-set set_a (cdr set_b) ) ) ) ) ) ) ) )

; From the previous section of the book. O(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


; 
;
; O(n). tree->list-2 is also O(n)
(define (union-tree tree-1 tree-2)
  (let ( (list-1 (tree->list-2 tree-1) )
         (list-2 (tree->list-2 tree-2) ) )
    (union-set list-1 list-2) ) )

; As above, O(n)
(define (intersection-tree tree-1 tree-2)
  (let ( (list-1 (tree->list-2 tree-1) )
         (list-2 (tree->list-2 tree-2) ) )
    (intersection-set list-1 list-2) ) )

; let's test our union implementation on a few trees
(define tree-1 (list->tree (list 1 3 5 7 9) ) )
(define tree-2 (list->tree (list 2 4 6 8 10) ) )
(define tree-3 (list->tree (list 1 2 3 4) ) )

(union-tree tree-1 tree-2)
(intersection-tree tree-1 tree-2)

;
(union-tree tree-1 tree-3)
(intersection-tree tree-1 tree-3)

;
(union-tree tree-2 tree-3)
(intersection-tree tree-2 tree-3)


