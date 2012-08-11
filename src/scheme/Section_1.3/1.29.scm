; SICP Exercise 1.29
;	Dawie de Klerk
;	2012-08-10

(load "../utils.scm")

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b) 0
	  (+ (term a)
		 (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-next x)
	(+ x h h))
  (define (simpson-term x)
	(+ (f x)
	   (* 4 (f (+ x h)))
	   (f (+ x h h))))
  (* (/ h 3)
	 (sum simpson-term a simpson-next b)))

(println "Simpson rule on x^3 on [0,1] with n=100:" (exact->inexact (simpson cube 0 1 100)))
(println "Simpson rule on x^3 on [0,1] with n=1000:" (exact->inexact (simpson cube 0 1 1000)))
