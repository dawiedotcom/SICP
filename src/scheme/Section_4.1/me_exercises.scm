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
    ;; The new eval that checks for application much earlier
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
  ;; The new abstraction for application
  (define (application? exp)
    (tagged-list? exp 'call))
  (define (operator exp)
    (cadr exp))
  (define (operands exp)
    (cddr exp))
  ;; Testing
  (eval '(call car '(1 2 3))
        the-global-environment))

;;; Exercise 4.4

;; OR expressions
(define (or? exp) (tagged-list? exp 'or))
(define (or-actions exp) (cdr exp))
(define (eval-or exp env) (eval-or-actions (or-actions exp) env))

(define (eval-or-actions actions env)
  ;; Eval each of the or actions according to the rules of OR.
  (if (null? actions)
      false
      (let ((res (eval (car actions) env)))
        (if (true? res)
            res
            (eval-or-actions (cdr actions) env)))))

;; AND expressions
(define (and? exp) (tagged-list? exp 'and))
(define (and-actions exp) (cdr exp))
(define (eval-and exp env) (eval-and-actions (and-actions exp) env))
(define (last-action? actions) (null? (cdr actions)))

(define (eval-and-actions actions env)
  ;; Eval each of the or actions according to the rules of AND.
  (if (null? actions)
      true
      (let ((res (eval (car actions) env)))
        (cond ((false? res) 'false)
              ((last-action? actions) res)
              (else
                (eval-and-actions (cdr actions) env))))))

;;; Exercise 4.5

(define (special-cond-clause? clause)
  (tagged-list? (cond-actions clause) '=>))
(define (cond-recipient clause)
  (caddr clause))

(define (expand-clauses clauses)
  ;; Converts a list of cond clauses to a set of nested IF
  ;; expressions.
  (if (null? clauses)
      'false                      ; cond with no ELSE clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               ; The ELSE clause. Should always be last and
               ; therefore expand to an IF expression's 
               ; alternative. The corner case
               ; (cond ((else ..))) will be a nop.
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "COND->IF -- ELSE clause isn't last" 
                          clauses)))

              ((special-cond-clause? first)
               ; Handle the special cond clauses
               (make-if (cond-predicate first)
                        (list 
                          (cond-recipient first)
                          (cond-predicate first))
                        (expand-clauses rest)))

              (else
               ; Normal clauses - make an IF expression with
               ; the other clauses recursivly in the alternative.
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (compare-eval)
  (define (test-all exprs)
    (if (not (null? exprs))
        (let ((e (car exprs))) 
          (display (eq? (eval-in-underlying-scheme 
                          e 
                          user-initial-environment)
                        (eval e the-global-environment)))
          (display " ")
          (display e)
          (display " => ")
          (display (eval e the-global-environment))
          (newline)
          (test-all (cdr exprs)))))
  (let ((tests
          (list 
            '(or true (display "hi"))
            '(or (+ 1 2) (+ 100 100))
            '(and true (+ 1 2))
            '(or)
            '(and)
            '(cond ((assoc 'b '((a 1) (b 2))) => cadr) 
                   (else false))
            )))
    (test-all tests)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
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
        ((or? exp) (eval-or exp env))
        ((and? exp) (eval-and exp env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "EVAL - Unknown expression type" exp))))

  

