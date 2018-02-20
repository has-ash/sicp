#lang sicp

; ex 2_30

(define (square-tree tree)
  (cond ( (null? tree) nil)
        ( (not (pair? tree) ) (* tree tree ) )
        (else
         (cons (square-tree (car tree) )
               (square-tree (cdr tree ) ) ) ) ) )

(define nested-list ( list 1
                           ( list 2
                                  ( list 3 4 ) 5 )
                           (list 6 7 ) ) )

;test
(square-tree nested-list)

; using higher order procedures

(define (square-tree-higher tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree) )
             (* sub-tree sub-tree)
             (square-tree-higher sub-tree) ) )
       tree) )

;test
(square-tree-higher nested-list)

; 2_31
; abstracting out our procedure

(define (tree-map function tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree) )
             (function sub-tree)
             (tree-map function sub-tree) ) )
       tree) )

(define (square x) (* x x ) )

; test
(tree-map square nested-list)
                             