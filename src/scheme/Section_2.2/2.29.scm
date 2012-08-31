; SICP Exercise 2.29
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; Selectors for mobiles
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

;; Selectors for branches
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))


;; 
;(define (total-weight mobile)
;  (let ((left-structure (branch-structure (left-branch mobile)))
;        (right-structure (branch-structure (right-branch mobile))))
;    (+ (if (number? left-structure) 
;           left-structure 
;           (total-weight left-structure))
;       (if (number? right-structure) 
;           right-structure 
;           (total-weight right-structure)))))

;; Calculating the weight
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (number? bs) bs
        (total-weight bs))))

;; Calculating torque
(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

;; Balanced predicates
(define (balanced? mobile)
  (let ((left-b (left-branch mobile))
        (right-b (right-branch mobile)))
    (and (= (torque left-b)
            (torque right-b))
         (balanced? (branch-structure left-b))
         (balanced? (branch-structure right-b)))))

(define (branch-balanced? branch)
  (if (number? (branch-structure branch))
      #t
      (balanced? (branch-stucture branch))))

;; Testing
(define (test)
  ;; My repl session looked something like
  (define b1 (make-branch 2 5))
  (define b2 (make-branch 1 10))
  (define b3 (make-branch 1 2)) 
  (define m1 (make-mobile b1 b2))
  (define b4 (make-branch 1 m1))
  (define m2 (make-mobile b1 b3))

  (print-eval b1)
  (print-eval b2)
  (print-eval b3)
  (print-eval b4)
  (print-eval m1)
  (print-eval m2)

  (print-eval (torque (make-branch 1 m1)))
  (print-eval (branch-weight b1))
  (print-eval (branch-weight (make-branch 1 m1)))
  (print-eval (balanced m1))
  (print-eval (balanced m2))
  (print-eval (balanced (make-mobile (make-branch 1 15) b4)))
  (print-eval (balanced (make-mobile (make-branch 1 14) b4))))


;; By redefining the constuctors to use cons instead of list,
;; we only need to redefine the selectors that use the second
;; element in the list representing branches and mobiles.
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))
(define (branch-structure branch)
  (cdr branch))
