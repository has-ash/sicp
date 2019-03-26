#lang racket

;ex 2.66
;
; Look up for a binary tree. This is just binary search on an array assuming
; that the records are placed in the array based on their keys. This should
; be O(log n) if the tree is relatively balanced.

; Let's do this for the trivial case where the nodes in our tree are just the keys.
; We can also do it the "right" way but that would require that we write additional code
; and that seems a bit excessive for such a simple exercise. ;)

; Alternatively make each tree entry a pair of key and some "data" and modify the accessor functions.
; Notice that for makingt the change, we need not change our lookup function. Just the accessors for the
; entries in the tree. 

(define (lookup given-key tree)
  (cond ( (null? tree) false)
        ( (eq? given-key (entry tree)) (get-data tree) )
        ( (> given-key (entry tree) ) (lookup given-key (right-branch tree) ) )
        (else (lookup given-key (left-branch tree) ) ) ) )

; And that's about it.
;(define (entry tree)
;  (car tree) )

(define (left-branch tree)
  (cadr tree) )

(define (right-branch tree)
  (caddr tree) )

;(define (get-data tree)
;  (entry tree) )

(define (make-tree root left right)
  (list root left right) )

(define tree_a (make-tree 5
                          (make-tree 3
                                     (make-tree 1 '() '() )
                                     (make-tree 4 '() '() ) )
                          (make-tree 7
                                     (make-tree 6 '() '() )
                                     (make-tree 8 '() '() ) )
                          ) )

(define key 5)

;(lookup 9 tree_a)

;(lookup 4 tree_a)


; Addition
; So let's modify our accessors and create a tree which has both key and data.

(define (make-tree-node key data)
  (cons key data) )

; entry tree still gives us key
(define (entry tree)
  (car (car tree) ) )

; get data now returns actual data in node.
(define (get-data tree)
  (cdr (car tree) ) )

; left-branch and right-branch are fine as they are.

(define tree_b (make-tree (make-tree-node 5 "five")
                          (make-tree (make-tree-node 3 "three")
                                     (make-tree (make-tree-node 1 "one") '() '() )
                                     (make-tree (make-tree-node 4 "four") '() '() ) )
                          (make-tree (make-tree-node 7 "seven")
                                     (make-tree (make-tree-node 6 "six") '() '() )
                                     (make-tree (make-tree-node 8 "eight") '() '() ) )
                          ) )

(lookup 9 tree_b)

(lookup 4 tree_b)
