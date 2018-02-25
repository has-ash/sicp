#lang sicp

; preliminaries

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high) ) ) )

(define (filter predicate sequence)
  (cond ( (null? sequence) nil )
        ( (predicate (car sequence)  )
          (cons (car sequence)
                (filter predicate (cdr sequence) ) ) )
        (else (filter predicate (cdr sequence) ) ) ) )

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )

; to be used for mapping (proc) : i is a value 
(define (list-of-pairs i)
  (map (lambda (j) (list i j) )
       (enumerate-interval 1 (- i 1) ) ) )

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence) ) )

;; prime
(define (square x)
  (* x x) )

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (( divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;


; predicate
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair) ) ) )

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair) ) ) )

; ex2_40

; the only thing we need to do is to pull the definition
; of the lambda function we pass to flatmap into unqiue-pairs.
; This is even easier since I already define list-of-pairs to do
; half the job.

(define (unique-pairs n)
  (flatmap list-of-pairs (enumerate-interval 1 n) ) )

; test

(unique-pairs 3)

; simplify prime-sum-pairs

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum?
                             (unique-pairs n) ) ) )

; test ---> (2,1) (3,2)

(prime-sum-pairs 3)


; ex 2_41

; We can use flatmap to accomplish this.
; Sequence operations: ordered triples -> filter

(define (ordered-triples n)
  (let ( (seq (enumerate-interval 1 n) ) )
  (flatmap (lambda (i)
             ; flatmap instead of map to get a list of lists
             ; map gives additional level
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k) )
                         seq ) )
                    seq ) )
           seq ) ) )

(ordered-triples 2)

(define (sum-of-triples? n s)
  (filter (lambda (triple)
            (= (+ (car triple) (cadr triple) (caddr triple) ) s ) )
          (ordered-triples n) ) )

(sum-of-triples? 2 4)

