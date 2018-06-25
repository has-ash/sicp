#lang sicp

; create the abstraction layer

(define (variable? x) (symbol? x) )

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2) ) )

(define (=number? exp num) (and (number? exp) (= exp num) ) )

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) )
         (+ a1 a2) )
        (else
         (list '+ a1 a2) ) ) )

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0) ) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) )
         (* m1 m2))
        (else
         (list '* m1 m2) ) ) )

(define (sum? a) (and (pair? a) (eq? (car a) '+ ) ) )

(define (addend a) (cadr a) )

(define (augend a) (caddr a) )

(define (product? a) (and (pair? a) (eq? (car a) '* ) ) )

(define (multiplier a) (cadr a) )

(define (multiplicand a) (caddr a ) )

;ex2_56

; use the symbol '** for exponentiation
(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**) ) ) 

(define (base a) (cadr a) )
(define (exponent a) (caddr a) )

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp) ) (expt base exponent) )
        (else (list '** base exp) ) ) )


; symbolic diff program

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponentiation
                                        (base exp)
                                        (make-sum (exponent exp) -1) ) )
          (deriv (base exp) var) ) )
        (else
         (error "unknown expression type: DERIV" exp))))

; some test expression
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'y)
(deriv '(* (* x y) (+ x 3)) 'x)

(define var1 (make-exponentiation 'u 'n) )
(deriv (deriv var1 'u) 'u) 

