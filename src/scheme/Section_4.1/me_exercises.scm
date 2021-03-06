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

;;; Exercise 4.6

(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-variables clauses) (map car clauses))
(define (let-exprs clauses) (map cadr clauses))

(define (let->combination exp)
  (let ((clauses (let-clauses exp)))
    (expand-let
      (let-variables clauses)
      (let-exprs clauses)
      (let-body exp))))

(define (expand-let vars exprs body)
  (cons
    (make-lambda vars (list body))
    exprs))

;;; Exercise 4.7

;; We can expand 
;;   (let* ((x 3)
;;          (y (+ x 2))
;;          (z (+ x y 5)))
;;     (* x z))
;; to
;;   (let ((x 3))
;;     (let ((y (+ x 2)))
;;       (let ((z (+ x y 5)))
;;         (* x z))))

;(define (let*-first-var clauses) (caar clauses))
;(define (let*-first-exp clauses) (cadar clauses))
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
    (expand-let* (let-clauses exp)
                 (let-body exp)))

(define (expand-let* clauses body)
  (if (null? clauses)
      body
      (let ((first (list (car clauses)))
            (rest (cdr clauses)))
        (expand-let
          (let-variables first)
          (let-exprs first)
          (expand-let* rest body)))))

;;; Exercise 4.8

(define (named-let? exp) (not (pair? (cadr exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-clauses exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (let ((clauses (named-let-clauses exp)))
        (expand-named-let
          (named-let-name exp)
          (let-variables clauses)
          (let-exprs clauses)
          (named-let-body exp)))
      (let ((clauses (let-clauses exp)))
        (expand-let
          (let-variables clauses)
          (let-exprs clauses)
          (let-body exp)))))

(define (expand-named-let name vars exprs body)
  (display "expand-named-let\n")
  (make-begin
    (list 
      (list 'define
            (cons name vars)
            body)
      (cons name exprs))))

;;; Exercise 4.11
;; An alternative representation for frames using hash-tables

(define (make-frame variables values)
  (let ((frame (make-eq-hash-table 
                 (* 2 (length variables)))))
    (map (lambda (var val)
           (add-binding-to-frame! var val frame ))
         variables
         values)
    frame))

(define (frame-variables frame)
  (hash-table/key-list frame))
(define (frame-values frame)
  (hash-table/datum-list frame))
(define (add-binding-to-frame! var val frame)
  (hash-table/put! frame var val))

;;; Exercise 4.12

(define (defined-in-frame? frame var)
  ;; Is it safe to assume no variable in a frame will 
  ;; have the frame as its value?
  (not (equal? (hash-table/get frame var frame) frame)))

(define (set-val-in-frame! var val frame)
  (hash-table/put! frame var val))
(define (get-val-in-frame var frame)
  (hash-table/get frame var #f))

(define (map-environment env var proc)
  ;; Calls proc with the frame it is defined in
  (define (map-e e)
    (if (eq? e the-empty-environment)
        (error "MAP-ENVIRONMENT -- Unbound variable" var)
        (let ((frame (first-frame e)))
          (if (defined-in-frame? frame var)
              (proc frame)
              (map-e (enclosing-environment e))))))
  (map-e env))

(define (lookup-variable-value var env) 
  (map-environment env 
                   var
                   (lambda (frame) 
                     (get-val-in-frame var frame))))

(define (define-variable! var val env) 
  (if (eq? env the-empty-environment)
      (error "DEFINE-VARIABLE! -- cannot define variables in the-empty-environment")
      (let ((frame (first-frame env)))   
        (if (defined-in-frame? frame var)
            (set-variable-value! var val env)
            (add-binding-to-frame! var val frame)))))

(define (set-variable-value! var val env)
  (map-environment env 
                   var
                   (lambda (frame) 
                     (set-val-in-frame! var val frame))))

;;; Exercise 4.13

;; New Frame procedures
(define (remove-binding-from-frame! frame var)
  (hash-table/remove! frame var))

;; Make-unbound! expression
(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound!))
(define (make-unbound-var exp)
  (cadr exp))

(define (eval-make-unbound! exp env)
  (let ((frame (first-frame env))
        (var (make-unbound-var exp)))
    (if (defined-in-frame? frame var)
        (remove-binding-from-frame! frame var)
        (error "MAKE-UNBOUND! -- var not defined in this frame" var))))

;;; Eval with all the new syntax expressions

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
            '(let ((x 2)) x)
            '(let ((x 2) (y 3)) (+ 2 3))
            '(let* ((x 3) (y (- x 2))) (* x y))
            '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
            '(begin
               (define (fib n)
                 (let fib-iter ((a 1)
                                (b 0)
                                (count n))
                   (if (= count 0)
                       b
                       (fib-iter (+ a b) a (- count 1)))))
               (fib 8))
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
        ;((make-unbound? exp) (eval-make-unbound! exp env))
        ((or? exp) (eval-or exp env))
        ((and? exp) (eval-and exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "EVAL - Unknown expression type" exp))))

