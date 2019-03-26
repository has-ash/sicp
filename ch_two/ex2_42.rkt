#lang sicp
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

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence) ) )


(define (length x)
 (accumulate (lambda (x y)
               (+ 1 y) )
             0
             x) )
(length nil)
(length (list 1 2 3) )

; ex2_4 positions as a row list with elements
;       denoting the row of the queen.

(define empty-board '() )

;reverse list - first position is row of queen in kth column
(define (adjoin-position new-row k rest-of-queens)
  (append (list new-row) rest-of-queens ) )

; k - column of new-queen, i - column of queen to compare with
(define (safe? k positions)
  (define (safe-iter? new-queen positions k i)
    (if (null? positions)
        #t
        (and (safe-one new-queen (car positions) k i)
             (safe-iter? new-queen (cdr positions) k (- i 1) ) ) ) )
  ; replicated condition - refactor? 
  (if (null? positions)
      #t
      (safe-iter? (car positions) (cdr positions) k (- k 1) ) ) )

(define (safe-one new-queen old-queen new-col old-col)
  (let ( (slope (/ (- new-queen old-queen ) (- new-col old-col) ) ) )
    (if (= new-queen old-queen)
        #f
        (not
         (or (= slope 1)
             (= slope -1) ) ) ) ) )

; test safe?

(display "TEST SAFE: EXP FALSE" )
(newline)
(safe? 4 (list 4 3 2 1) )

(display "TEST SAFE: EXP FALSE" )
(newline)
(safe? 2 (list 1 2) )

(display "TEST SAFE: EXP TRUE" )
(newline)
(safe? 5 (list 4 2 5 3 1) )


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; compare results with 
; http://oeis.org/A000170
; the number of ways to placing n nonattacking queens on an n X n board are:
; 1 0 0 2 10 4 40 92 352 724
(map (lambda (x)
       (display (length (queens x) ) )
       (newline) )
     (enumerate-interval 1 10) )


;ex 2.43

; Since we now do the recurisve call queen-cols once for each element in
; enumerate, at each recursion level, we duplicate the work
; and so we get T + 2T + 3T + .... nT  ---> approx. n^2 T 
