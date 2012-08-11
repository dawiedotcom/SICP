; SICP Exercise 1.21
;	Dawie de Klerk
;	2012-08-08

(load "../utils.scm")

(define (smallest-divisor n)
  (find-divisor n 2))
  
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (zero? (remainder b a)))

(define (square x)
  (* x x))

(println 199 (smallest-divisor 199))
(println 1999 (smallest-divisor 1999))
(println 19999 (smallest-divisor 19999))
