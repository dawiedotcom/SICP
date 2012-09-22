; SICP Exercise 2.73
;   Dawie de Klerk
;   2012-09-22

(load "../utils.scm")

(define (deriv exp var)
  (println "DERIV:" exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp)
                                      var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? exp)
  (symbol? exp))
(define (same-variable? exp var)
  (eq? exp var))
;;; a. 

(define *deriv-list* '())

(define (put op type item)
  (set! *deriv-list*
        (cons (cons (cons op type) item) *deriv-list*)))

(define (get op type)
  (let ((key (cons op type)))
    (let ((result (filter (lambda (item) (equal? (car item) key))
                  *deriv-list*)))
      (if (null? result)
          #f
          (cdar result)))))

(define (install-deriv-package)
  ;; Internal functions
  (define (zero? a) (and (number? a) (= a 0)))
  (define (one? a) (and (number? a) (= a 1)))
  ;; sumation
  (define (make-sum s1 s2) 
    (cond ((zero? s1) s2)
          ((zero? s2) s1)
          ((and (number? s2) (number? s1))
           (+ s1 s2))
          (else
           (list '+ s1 s2))))
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  ;; multiplication
  (define (make-product m1 m2)
    (cond ((one? m1) m2)
          ((one? m2) m1)
          ((or (zero? m2) (zero? m1)) 0)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else
           (list '* m1 m2))))
  (define (multiplier exp) (car exp))
  (define (multiplicant exp) (cadr exp))
  ;; exponentiation
  (define (make-exponent base exponent)
    (cond ((one? exponent) base)
          ((zero? exponent) 1)
          (else
           (list '** base exponent))))
  (define (base e) (car e))
  (define (exponent e) (cadr e))

  ;; Deriv operations
  (define (deriv-sum exp var)
    ;(println "DERIV-SUM" exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    ;(println "DERIV-PRODUCT" exp var)
    (make-sum 
      (make-product (multiplier exp)
                    (deriv (multiplicant exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicant exp))))
  (define (deriv-exponent exp var)
    ;(println "DERIV-EXPONENT" exp var)
    (make-product 
      (exponent exp)
      (make-exponent
        (base exp)
        (- (exponent exp) 1))))
  ;; Interface with the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent)
  'done)

;(define (intstall-deriv-trig-package)

;  (define (deriv-sin exp var)

;  (put 'deriv 'sin deriv-sin)
;  (put 'deriv 'cos deriv-cos)
;  'done)

(install-deriv-package)
