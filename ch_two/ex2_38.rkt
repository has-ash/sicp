#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest) )
              (cdr rest) ) ) )
  (iter initial sequence) )

; expected : 3 / 2 and 1 / 6
(accumulate / 1 (list 1 2 3) )
(fold-left / 1 (list 1 2 3) )

; expected:
;                 ( 1 ( 2 ( 3 '() '() ) '() ) '() )
;    ( ( ( '() 1 '() ) 2 '() ) 3 '() )
(accumulate list nil (list 1 2 3) )
(fold-left list nil (list 1 2 3) )

; One property that immediately jumps out is associativity
; since we're doing operations in a different order and
; that means the operations should be associative for the result to be
; the same if we start from either end. 
