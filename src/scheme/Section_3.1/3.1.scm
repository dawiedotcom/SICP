; SICP Exersice 3.1
;   Dawie de Klerk
;   2012-09-30

(load "../utils.scm")

(define (make-accumulator sum)
  (lambda (addend)
    (begin (set! sum (+ sum addend))
           sum)))

(define (do-example)
  (let ((a (make-accumulator 5)))
    (println "(define A (make-accumulator 5))")
    (print-eval (a 10))
    (print-eval (a 10))))
