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

(display "ex2_67 decode message result:\n")

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

;(symbol-in-tree? 'A sample-tree)

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
(display "ex2_68 - encoded message followed by true message (should be same):\n")
(encode decoded-message sample-tree)
sample-message


; ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; solution

(define (successive-merge ordered-set)
  (if (null? (cdr ordered-set) )
      (car ordered-set)
      (successive-merge (adjoin-set
                         (make-code-tree
                          (car ordered-set)
                          (cadr ordered-set) )
                         (cddr ordered-set) ) ) ) )

;; test successive-merge
(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1) ) )

(define leaf-set-of-pairs (make-leaf-set pairs) )

(display "ex2_69 - successive merge result: \n")
(successive-merge leaf-set-of-pairs )
(display "for pairs: \n")
pairs

; ex 2.70
 
(define music-pairs (list (list 'a 2)
                          (list 'boom 1)
                          (list 'Get 2)
                          (list 'job 2)
                          (list 'Sha 3)
                          (list 'na 16)
                          (list 'Wah 1)
                          (list 'yip 9) ) )

(define lyrics-to-be-encoded (list 'Get 'a 'job
                                   'Sha 'na 'na 'na 'na 'na 'na 'na 'na
                                   'Get 'a 'job
                                   'Sha 'na 'na 'na 'na 'na 'na 'na 'na
                                   'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip
                                   'Sha 'boom) )

(define music-huffman-tree (generate-huffman-tree music-pairs) )

(define (count-list-elements a-list)
  (define (helper a-list count)
    (if (null? a-list)
        count
        (helper (cdr a-list) (+ count 1) ) ) )
  (helper a-list 0 ) )

(display "ex 2_70 - number of bits required: \n")
(count-list-elements (encode lyrics-to-be-encoded music-huffman-tree) )

;; with a fixed length code, since we have 8 = 2^3 symbols, we neeed 3 bits for each
;; symbol. This gives a total of 3 * 36 = 108 bits. 

;; ex 2_71

;; the huffman tree is like a list (very unbalanced)
;; we get 1 bit for the most frequent symbol (first leaf node is 1 level deep)
;; and n-1 bits for the least frequent symbol(s). You can see this in this sketch of
;; of a small huffman tree

;;           (1,2,4,8)
;;           /        \
;;          /         (8)
;;         (1,2,4)
;;        /      \
;;       /       (4)
;;     (1,2)
;;     /    \
;;    (1)   (2)
