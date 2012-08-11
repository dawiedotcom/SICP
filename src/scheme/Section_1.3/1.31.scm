; SICP Exercise 1.29
;	Dawie de Klerk
;	2012-08-10

(define (product term a next b)
  (define (iter a result)
	(if (> a b) result
	    (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc x) (+ 1 x))

(define (factorial n)
  (product identity 1 inc n))


(println "2! =" (factorial 2))
(println "3! =" (factorial 3))
(println "4! =" (factorial 4))
(println "5! =" (factorial 5))

(define (pi/4 m)
  (define (pi-term n)
	; Group 4 numbers into a term. For n=1: (2.4)/(3.3), etc
	(define n2 (* 2 n))
	(exact->inexact (* n2 (+ 2 n2) (/ 1 (square (+ n2 1))))))
  (product pi-term 1 inc m))
;(define pi (* (pi/4 100) 4))

(println "pi =" (* (pi/4 10) 4) " with 10 iterations")
(println "pi =" (* (pi/4 100) 4) " with 100 iterations")
(println "pi =" (* (pi/4 1000) 4) " with 1000 iterations")
