#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )

(define (filter predicate sequence)
  (cond ( (null? sequence) nil )
        ( (predicate (car sequence) )
          (cons (car sequence)
                filter predicate (cdr sequence) ) )
        (else (filter predicate (cdr sequence) ) ) ) )


; ex 2_33
;a) we only have to apply p to the first element (car sequence)
;   try unrolling the recursive calls.
(define (map-new p sequence)
  (accumulate (lambda (x y) (cons (p x) y ) )
              nil sequence) )

(define seq (list 1 2 3 4 5) )
(define (square x) (* x x ) )

(map-new square (list 1 2 3 4 5) )

; b)

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1) )

(define s1 (list 1 2 3) )
(define s2 (list 4 5 6) )

(append-new s1 s2)

; c)
; x is car sequence
; y is where I'll get the first 0 and what I should add to.
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y) ) 0 sequence ) )

(length s1)
(length seq) 
