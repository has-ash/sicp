#lang sicp

(define (memq item x)
  (cond ( (null? x) false)
        ( (eq? item (car x) ) x)
        (else (memq item (cdr x) ) ) ) )

(memq 'apple '(pear banana prune) )

(memq 'apple '(x (apple sauce) y apple pear) )

; ex2_53

; 1. ( mcons 'a (mcons 'b ....

(list 'a 'b 'c)

; 2. (mcons (mcons 'george '() ) '() )

(list (list 'george) )

; 3 ('(y1 y2) ) : Draw explicit representation of list
;                 to see outer list

(cdr '( (x1 x2) (y1 y2) ) )

; 4 '(y1 y2)

(cadr '( (x1 x2) (y1 y2) ) )

; 5 #f - car is 'a

(pair? (car '(a short list) ) )

;6 #f

(memq 'red '((red shoes) (blue socks) ) )

;7 '(red shoes blue socks)

(memq 'red '(red shoes blue socks) )

; ex2_54

; straightforward implementation
(define (are_equal? x y)
  ; if one is pair the other should also be pair
  (cond ( (pair? x)
          (and (are_equal? (car x) (car y) )
               (are_equal? (cdr x) (cdr y) ) ) )
        (else (eq? x y) ) ) )

(are_equal? '(this is a list) '(this is a list) )
(are_equal? '(this is a list) '(this (is a) list) )

; ex 2_55

; This happens since the expression expands to (quote (quote abracadabra) )
; and the first quote "quotes" everything afterwards thereby giving us '(quote abracadabra)
; which is a list with two quoted elements.
