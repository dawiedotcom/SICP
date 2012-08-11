; SICP Exercise 1.29
;	Dawie de Klerk
;	2012-08-10


(load "../utils.scm")

(define (sum term a next b)
  (define (iter a result)
	(if (> a b) result
	    (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (identity x) x)
(define (inc x) (+ x 1))

(println "Sum 1,...,10:" (sum identity 1 inc 10))
