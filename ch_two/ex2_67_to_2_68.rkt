#lang sicp 


; some preliminaries
; define leaf and accessors
(define (make-leaf symbol weight) (list 'leaf symbol weight ))
(define (leaf? object) (eq? (car object) 'leaf) )
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; trees and accessors
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right) )
        (+ (weight left) (weight right) ) ) )

(define (left-branch tree) (car tree) )
(define (right-branch tree) (cadr tree) )

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree ) )
      (caddr tree) ) )

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree) ) )


; decoding

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE BRANCH" bit ) )))

; ordered representation for merging (forming a huffman tree)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set) )
        (else (cons (car set)
                    (adjoin-set x (cdr set) ) ) ) ) )

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)  ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs)) )) ))


; ex2_67 decode a message

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1) ) ) ) )

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0 ))

; use decode to decode the message and give the result

(decode sample-message sample-tree)
(define decoded-message (decode sample-message sample-tree) ); for use later

; ex2_68 encode-symbol

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree )
      (encode (cdr message) tree ) ) ) )

; call on root
(define (in-list? elem list)
  (cond ((null? list) #f)
        ((eq? elem (car list) ) #t)
        (else (in-list? elem (cdr list) ) )  ) ) 

(define (symbol-in-tree? symbol tree)
  (let ((tree-symbols (symbols tree) ))
    (in-list? symbol tree-symbols) ) )

(symbol-in-tree? 'A sample-tree)

(define (enc-symb symb tree bits)
  (if (leaf? tree)
      bits
      (let ((left (left-branch tree) )
            (right (right-branch tree) ) )
            (cond ((symbol-in-tree? symb left)
                   (enc-symb symb left (append bits (list 0) ) ) )
                  (else
                   (enc-symb symb right (append bits (list 1) ) ) ) ) ) ) )

(define (encode-symbol symbol tree)
  (if (eq? (symbol-in-tree? symbol tree) #f)
      (error "symbol not in tree")
      (enc-symb symbol tree '() ) ) )


; test
;(encode-symbol 'Z sample-tree)
;(encode-symbol 'D sample-tree)

; encode decoded symbols
(encode decoded-message sample-tree)
sample-message
      


 





