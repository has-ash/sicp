#lang sicp

; ex 2_27 deep reverse
; reverse implementation from 2_18

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) ) ) )

(define (reverse x)
  (if (null? x)
       x 
      (append (reverse (cdr x) ) (cons (car x ) (list) ) ) ) )
; ex2_27
;
; to implement deep reverse we have to see if  car x
; is itself a list. If so, we have to append the reverse of that with
; the deep reverse of the cdr. 

(define (deep-reverse x)
  (cond ( (null? x) x )
        ; if car x is a list itself then take its reverse i.e. deep reverse
        ( (list? (car x ) ) (append (deep-reverse (cdr x) ) (cons (reverse (car x ) ) '() ) ) )
        ( else (append (reverse (cdr x) ) (cons (car x ) (list) ) ) ) ) )

(define a (list (list 1 2) (list 3 4 ) ) )
(define b (list (list (list 1 2) 3 4 ) 5 6) )
b
(reverse b)
(deep-reverse b)

;(reverse a) 
;(deep-reverse a)
