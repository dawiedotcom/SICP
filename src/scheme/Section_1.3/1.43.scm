; SICP Exercise 1.43
;   Dawie de Klerk
;   2012-08-13

(load "../utils.scm")
(load "1.42.scm")

(define (repeated f n)
  ;; Returns the n-th repeated application of f.
  (define (iter count result)
    (if (zero? count) result
        (iter (dec count) (compose f result))))
  (iter n (lambda (x) x)))

(define (test-repeated)
  (println "((repeated square 2) 5)\n"
           ((repeated square 2) 5)))
