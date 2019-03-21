#lang sicp
(#%provide (all-defined) )

; ex2-63
; tree-list 1

; Both list->tree-1 and tree->list-2 seem to give the same output.
; However they have different time complexities since list-1 uses
; append, which means at each level of the tree, we have to
; walk through the cons of the root and the right hand side to
; append it to the left hand side.
; In general, if we have n-1 elements where n = 2^k, ---> k = log (n)
; then at the lowest level of the tree, at the leaves, we will do a
; constant amount of work ( appending the lhs '() to cons (elems '() )
; should be constant time ) for n/2 leaf elements. At the level above this,
; we would have n/4 elements. For each of these elements, we would append
; the lhs, an elements, with a cons of two elements. This means that for each node
; here we perform 2 units of work. Continuing in this vein, we get a total amount of
;
; Total_work = n/2 + n/4 * (2) + n/8 * (4) + .... = n * (1/2 + 1/2 + 1/2 ... i.e. k of these)
;            = k/2 * n = O( n * log n )
;
; list->tree-2, however does not used append. Only cons. Therefore, we should expect a total
; amount of work of O(n) -- we do a constant amount of work for each element in the tree. 
;
; 
; 



; some preliminaries
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

; tree list 2

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define some_tree (make-tree 5 (make-tree 4
                                          (make-tree 3 '() '() )
                                          (make-tree 2 '() '() ) )
                             (make-tree 6
                                        (make-tree 7 '() '() )
                                        (make-tree 8 '() '() ) ) ) )
;some_tree
;(tree->list-1 some_tree)
;(tree->list-2 some_tree)


 