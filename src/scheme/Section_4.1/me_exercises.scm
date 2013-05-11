; SICP Section 4.1
;   Dawie de Klerk
;   2013-05-09

;(load "me.scm")

;;; Exercise 4.1

(define (test-evaluation-order)
  ;; This is a rather elaborate experiment to check the order in which 
  ;; the metacircular evaluator's EVAL evaluates arguments passed to 
  ;; procedures. A dummy environment is setup where the LIST procedure 
  ;; keeps track of all arguments passed to it. The fake LIST is called 
  ;; twice, as arguments to EQ?, and then we check which call to LIST 
  ;; was evaluated first.

  (let ((fake-list-args '()))
    (define (fake-list . args)
      ;; A LIST procedure that keeps track of all its arguments in 
      ;; fake-list-args
      (set! fake-list-args (cons args fake-list-args))
      (apply-in-underlying-scheme list args))
    (let ((env (extend-environment 
                 ; The dummy environment that where list in the
                 ; metacircular evaluator calls fake-list in the
                 ; base implementation
                 (list 'eq? 'list)
                 (list 
                   (list 'primitive eq?)
                   (list 'primitive fake-list))
                 the-empty-environment)))

      (eval '(eq? (list 1) (list 2))
                     env)

      ;(display fake-list-args) (newline)
      (if (= (caar fake-list-args) 1)
          ; (1) at the car means (list 1) was evaled last.
          'right-to-left-evaluation
          'left-to-right-evaluation))))

(define (list-of-values exps env)
  ;; Always right to left evaluation by evaluating the
  ;; recursive step first.
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))

(define (list-of-values exps env)
  ;; Always left to right evaluation by evaluating the
  ;; left most argument first
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first
              (list-of-values (rest-operands exps) env)))))

;;; Exercise 4.2

;; a) Procedure application is represented by a list. Most other
;;    expression types are represented by tagged lists,  
;;    definitions in particular. So any expression represented
;;    as a tagged must be tested for before procedure application
;;    to prevent them from being interpreted as procedure applications. 
;;    Something like (define x 3) will be interpreted as apply the 
;;    procedure DEFINE on arguments x and 3.

;; b) We can implement Louis's plan by representing application by 
;;    the tagged list (call proc args)

(define (do-4-2)
  (define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          ((variable? exp)
           (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          (else
            (error "EVAL - Unknown expression type" exp))))
  (define (application? exp)
    (tagged-list? exp 'call))
  (define (operator exp)
    (cadr exp))
  (define (operands exp)
    (cddr exp))
  (eval '(call car '(1 2 3))
        the-global-environment))

