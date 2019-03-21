#lang sicp
(#%provide (all-defined) )
(#%require "./ex2_63.rkt")

; some preliminaries

; (a)
;   How do you build a tree? You divide the list in halves (halvish minus the root), build the
;  (make-tree root partial-left-tree partial-right-tree)
;  and you're done if you do this recursively. That's exactly what partial tree does.
;
; (b)
;  The function list->tree visits each element in the list once and performs a constant amount
; of work for each ( calls make-tree ). Assuming that make-tree doesn not scale with the size of the
; trees we pass it to, i.e. only does a constant amount of work, no matter how large the arguments are,
; the time complexity ought to be O(n).
;

(define (list->tree elements)
(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;(define my_list (list 1 3 5 7 9 11) )
;(list->tree my_list)