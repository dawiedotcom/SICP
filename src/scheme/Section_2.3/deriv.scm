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

;;; Sums -- Using two arguments in infix notation
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

;;; Sums -- Using arbitrary number arguments in infix notation
;;;   The problem here is with the order in which operations 
;;;   are given. Sums should always be differentiated first 
;;;   (fortunately they are in deriv), but our selectors and
;;;   predicates should be aware of this. For instance in
;;;   (3 * x + 4), we should differentiate the summation first and
;;;   then the product. Therefore it is no longer sufficient to
;;;   check if the second element is the symbol +. A sum is any
;;;   list containing the symbol +. The addend is everything before
;;;   + and the augend everything after.
(define (term? sym expr)
  (and (pair? expr)
       (pair? (memq sym expr))))
(define (before sym s)
  (let ((head (list-head s
                        (- (length s)
                           (length (memq sym s))))))
    (if (null? (cdr head))
        (car head)
        head)))
(define (after sym s)
  (let ((items (memq sym s)))
    (if (null? (cddr items))
        (cadr items)
        (cdr items))))

(define (sum? s)
  (term? '+ s))
(define (addend s)
  (before '+ s))
(define (augend s)
  (after '+ s))
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
  ;; For two term infix
  (caddr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

;;; Products -- Using aribtrary number arguments in infix notation
(define (product? s)
  (term? '* s))
(define (multipler s)
  (before '* s))
(define (multiplicand s)
  (after '* s))

;;; Exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else 
          (list base '** exponent))))
(define (exponentiation? e)
  (and (pair? e)
       (eq? (cadr e) '**)))
(define (base ex)
  (car ex))
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
  (print-eval (prefix->infix (deriv '(+ (** x 2) (** x 3) (** x 4)) 'x))))

(define (do-infix-examples)
  (print-eval (deriv '(x * 3 + 5 * (x + y + 2)) 'x))
  (print-eval (deriv '(x + 3 * (x + y + 2)) 'x))
  (print-eval (deriv '(4 * x ** 3 + 2 * x ** 2 + 3 * x) 'x)))
