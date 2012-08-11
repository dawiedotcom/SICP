; SICP Exercise 1.17
; 	Dawie de Klerk
; 	2012-08-06

(define (double a) (+ a a))
(define (half a) (/ a 2))
(define (one? a) (= a 1))

(define (mul a b)
  (cond ((zero? b) 0)
		((one? b)  a)
		((even? b) (double (mul a (half b))))
		(else (+ a (mul a (- b 1))))))
