; SICP Exercise 2.31
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (tree-map proc tree)
  ;; Map proc over each element in tree
  (map (lambda (elem)
         (if (list? elem)
             (tree-map proc elem)
             (proc elem)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define (repl-test)
  (print-eval (square-tree '(1 (2 (3 4) 5) (6 7)))))
