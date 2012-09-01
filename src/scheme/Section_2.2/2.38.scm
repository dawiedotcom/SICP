; SICP Exercise 2.38
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")
(load "2.37.scm")

(define (rand-mat-33)
  (list (list (random 10) (random 10) (random 10))
        (list (random 10) (random 10) (random 10))
        (list (random 10) (random 10) (random 10))))

(define (run-examples)
  (print-eval (fold-right / 1 (list 1 2 3)))
  (print-eval (fold-left / 1 (list 1 2 3)))
  (print-eval (fold-right list '() (list 1 2 3)))
  (print-eval (fold-left list '() (list 1 2 3)))

  ;; fold-left and fold-right will give the same result 
  ;; if op is commutative i.e. (op a b) = (op b a)
  ;; EDIT: op must be associative too, see
  ;;  [1]: http://www.billthelizard.com/2011/04/sicp-238-239-folding-left-and-right.html?showComment=1342250199834#c3927189465395450595

  ;; Fold left calculates:
  ;; (fold-left op a '(b c d))
  ;; (iter a '(b c d))
  ;; (iter (op a b) '(c d))
  ;; (iter (op (op a b) c) '(d))
  ;; (iter (op (op (op a b) c) d) '())
  ;; (op (op (op a b) c) d)
  ;;
  ;; Fold right calculates:
  ;; (fold-right op a '(b c d))
  ;; (op b (fold-right op a '(c d)))
  ;; (op b (op c (fold-right op a '(d))))
  ;; (op b (op c (op d (fold-right op a '()))))
  ;; (op b (op c (op d a)))
  ;;
  ;; Suppose op is only commutative: (op x y) = (op y x):
  ;;
  ;; (op b (op c (op a d)))
  ;; (op b (op (op a d) c))
  ;; (op (op (op a d) c) b)
  ;;
  ;; Suppose op is only associative: (op x (op y z)) = (op (op x y) z)
  ;;
  ;; (op b (op c (op a d)))
  ;; (op b (op (op c a) d))
  ;; (op (op b (op c a)) d)
  ;; (op (op (op b c) a) d)
  ;;
  ;; Suppose op is both comutative and associative
  ;;
  ;; (op b (op c (op a d)))
  ;; (op b (op (op c a) d))   associative with innermost op
  ;; (op b (op (op a c) d))   commute a and c
  ;; (op (op b (op a c)) d)   associative with middle op
  ;; (op (op (op b a) c) d)   commute b with innermost op
  ;; (op (op (op a b) c) d)   commute a b

  (print-eval (fold-right + 0 (list 1 2 3 4)))
  (print-eval (fold-left + 0 (list 1 2 3 4)))
  (print-eval (fold-right * 1 (list 1 2 3 4)))
  (print-eval (fold-left * 1 (list 1 2 3 4)))

  ;; Do some tests with matrices
  (let ((init (rand-mat-33))
        (a1 (rand-mat-33))
        (a2 (rand-mat-33)))
    (print-eval init)
    (print-eval a1)
    (print-eval a2)
    (print-eval (fold-right matrix-*-matrix init (list a1 a2)))
    (print-eval (fold-left matrix-*-matrix init (list a1 a2))))

  ;; Do some tests with average
  (define (average a b) (/ (+ a b) 2.))
  (print-eval (fold-right average 1 (list 1 2 3 4 5)))
  (print-eval (fold-left average 1 (list 1 2 3 4 5))))



