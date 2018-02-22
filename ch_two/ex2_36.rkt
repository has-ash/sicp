#lang sicp

;ex 2.36

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence) ) ) ) )

; seqs - sequence of sequences

(define (accumulate-n op init seqs)
  (if (null? (car seqs) )
      nil
      (cons (accumulate op init (map (lambda (x) (car x) ) seqs ) )
            (accumulate-n op init (map (lambda (x) (cdr x) ) seqs ) ) ) ) )

(define seq-of-seq (list (list 1 2 3) (list 4 5 6) (list 5 6 7) ) )

(accumulate-n + 0 seq-of-seq)