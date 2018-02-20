#lang sicp

; ex 2_29

(define (make-mobile left right)
  (list left right ) )

(define (make-branch length structure)
  (list length structure) )

; a) 
(define (left-branch mobile)
  (car mobile) )

(define (right-branch mobile)
  (car (cdr mobile ) ) )

; test functions
(define branch-one (make-branch 1 4) )
(define branch-two (make-branch 2 5) )
(define mobile (make-mobile branch-one branch-two) )

(left-branch mobile)
(right-branch mobile)

; b)

(define (get-structure branch)
  (car (cdr branch) ) )

; test get-structure
(get-structure branch-one)
(get-structure branch-two)

(define branch-three (make-branch 3 mobile) )
(define branch-four (make-branch 4 mobile) )

; test get-structure with new branches
(eqv? (get-structure branch-three) mobile)
(eqv? (get-structure branch-four) mobile)

(define complex-mobile (make-mobile branch-three branch-four) )

; another battery of tests
(eqv? (get-structure (left-branch complex-mobile) ) mobile)

; get-weight returns the weight of a structure which is either a number
; or a mobile
(define (get-weight structure)
  (if (not (pair? structure) )
      structure
      ; otherwise it is a mobile
      (+ (get-weight (get-structure (left-branch structure) ) )
         (get-weight (get-structure (right-branch structure) ) ) ) ) )

(newline)
(display "expected 4")
(newline)
(get-weight (get-structure (left-branch mobile) ) )

; just a wrapper around get-weight
(define (total-weight mobile)
 (let ( (left-structure (get-structure (left-branch mobile) ) )
         (right-structure (get-structure (right-branch mobile) ) ) )
   (+ (get-weight left-structure)
      (get-weight right-structure) ) ) )

(newline)
(display "FINAL TESTs : expected vals 9 and 18" )
(newline)
(total-weight mobile)
(total-weight complex-mobile)

; c)

(define (get-length branch)
  (car branch) )

(define (torque branch)
  (let ( (length (get-length branch) )
         (weight (get-weight (get-structure branch) ) ) )
  (* length weight ) ) ) 

(define (branch-balanced? branch)
  (let ( (structure (get-structure branch) ) )
    (if (pair? structure)
        (balanced? mobile)
        #t ) ) )   
         
(define (balanced? mobile)
  (let ( (left (left-branch mobile) )
         (right (right-branch mobile) ) )
    (and (= (torque left)
            (torque right) )
         (branch-balanced? left)
         (branch-balanced? right) ) ) )

(balanced? mobile)

(define balanced-mobile (make-mobile branch-one branch-one) )
(balanced? balanced-mobile)

; d) Just the selectors
; Nothing else needs to change since everything else depends on the selectors