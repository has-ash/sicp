#lang sicp

; function defs
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )

(define (accumulate-n op init seqs)
  (if (null? (car seqs) )
      nil
      (cons (accumulate op init (map (lambda (x) (car x) ) seqs ) )
            (accumulate-n op init (map (lambda (x) (cdr x) ) seqs ) ) ) ) )



; ex 2_37

(define (dot-product v w)
  (accumulate + 0 (map * v w) ) )

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v ) )
       m ) )

(define (transpose mat)
  (accumulate-n cons nil mat) )

; first row of output is the product of 
; the transpose of n with the first row of m and so on
(define (matrix-*-matrix m n)
  (let ( (cols (transpose n) ) )
    (map (lambda (x)
           (matrix-*-vector cols x ) )m) ) )

;tests
(define vector (list 1 2 3 ) )
(define matrix-a (list (list 1 2 3) (list 4 5 6) (list 7 8 9) ) )
(define matrix-b (list (list 1 0 0) (list 0 1 0) (list 0 0 1) ) )

; expected : 14
(dot-product vector vector)

;expected (14 32 50)
(matrix-*-vector matrix-a vector)

;expected ( (1 4 7) (2 5 8) (3 6 9) )
(transpose matrix-a)

;expected (matrix-a)
(matrix-*-matrix matrix-a matrix-b)
(matrix-*-matrix matrix-b matrix-a)