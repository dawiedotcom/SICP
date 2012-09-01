; SICP Exercise 2.39
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")
(load "2.36.scm")

(define (reverse seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

(newline)
(println "Using reverse with fold-right")
(print-eval (reverse (list 1 2 3 4 5 6)))

(define (reverse seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(newline)
(println "Using reverse with fold-left")
(print-eval (reverse (list 1 2 3 4 5 6)))
