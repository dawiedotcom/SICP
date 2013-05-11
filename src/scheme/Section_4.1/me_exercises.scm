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
