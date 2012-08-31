; SICP Exercise 2.34
;   Dawie de Klerk
;   2012-08-31

(load "../utils.scm")
(load "2.33.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms)
                (+ (* higher-terms x) this-coef))
              0
              coefficient-sequence))

(define (testish)
  (print-eval (horner-eval 2 (list 1 3 0 5 0 1))))
