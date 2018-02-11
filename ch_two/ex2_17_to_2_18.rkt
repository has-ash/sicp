#lang sicp


;ex 2_17
; x is a non empty list
(define (last-pair x)
  (if (null? (cdr x) )
      ;return list not element
      (cons (car x) (list) )
      (last-pair (cdr x) ) ) )

(define squares (list 1 4 9 16 25 36) )
(define odds (list 1 3 5 7 9 11) )

(last-pair squares)

; ex 2_18
;reverse using append

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) ) ) )

(append squares odds)

(define (reverse x)
  (if (null? x)
       x 
      (append (reverse (cdr x) ) (cons (car x ) (list) ) ) ) )

(reverse squares)
(reverse (list) )