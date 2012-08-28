; SICP Exercise 2.27
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (square-tree tree)
  ;; Square tree using map and recursion
  (map (lambda (elem)
         (if (list? elem)
             (square-tree elem)
             (square elem)))
       tree))

(define (square-tree tree)
  ;; Direct implementation of square tree
  (define (iter acc items)
    (cond ((null? items) acc)
          ((number? (car items))
            (iter 
              (append acc (list (square (car items)))) 
              (cdr items)))
          (else
             (iter
               (append acc (square-tree (car items)))
               (cdr items)))))
  (iter '() tree))
