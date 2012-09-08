; SICP Exercises 2.56 to 2.58
;   Dawie de Klerk
;   2012-09-05

(load "../utils.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
           (exponent exp)
           (make-product
             (make-exponentiation (base exp)
                                  (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else
          (error "unknown expression type -- DERIV" exp))))


;;;; Representation of expressions

;;; Variables
(define (variable? x) 
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;;; Sums -- Using an arbitrary number of arguments in prefix notation
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) 
              (number? a2)) 
         (+ a1 a2))
        (else 
          (group-terms '+ sum? a1 a2))))
(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

;;; Products -- Using two arguments in infix notation
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) 
              (number? a2)) 
         (+ a1 a2))
        (else 
          (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))
(define (addend s)
  (car s))
(define (augend s)
  (caddr s))

;;; Products -- Using an arbitrary number of arguments in prefix notation
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) 
              (number? m2))
         (* m1 m2))
        (else
          (group-terms '* product? m1 m2))))
      
(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;;; Products -- Using two arguments in infix notation
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) 
              (number? m2))
         (* m1 m2))
        (else
          (list m1 '* m2))))
      
(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (caddr p))

;;; Exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else 
          (list '** base exponent))))
(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '**)))
(define (base ex)
  (cadr ex))
(define (exponent ex)
  (caddr ex))

;;; Helpers
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (group-terms sym test? . terms)
  ;; Combines multiple expressions of sym into one.
  (cons sym
    (fold-left
      (lambda (acc next)
        (cond ((test? next) (append acc (cdr next)))
              ;((list? next) (append acc next))
              (else (append acc (list next)))))
      '()
      terms)))

(define (prefix->infix expr)
  ;; Convert an expression in prefix notation to infix notation
  (let ((sym (list (car expr)))
        (params (cdr expr)))
    (fold-left
      (lambda (acc next)
        (if (pair? next)
            (append acc sym (to-infix next))
            (append acc sym (list next))))
      (if (pair? (car params))
                (to-infix (car params))
                (list (car params)))
      (cdr params))))

;;; TEST
(define (do-examples)
  (print-eval (deriv '(+ x 3) 'x))
  (print-eval (deriv '(* x y) 'x))
  (print-eval (deriv '(* (* x y) (+ x 3)) 'x))
  (print-eval (deriv '(** x 3) 'x))
  (print-eval (deriv '(** (* x y) 2) 'x))
  (print-eval (to-infix (deriv '(+ (** x 2) (** x 3) (** x 4)) 'x))))
