; SICP Exercise 1.18
; 	Dawie de Klerk
; 	2012-08-06

(define (double a) (+ a a))
(define (half a) (/ a 2))

(define (mul a b)
  (define (mul-iter a b n)
	(cond ((zero? b) n)
		  ((even? b) (mul-iter (double a) (half b) n))
		  (else (mul-iter a (- b 1) (+ n a)))))
  (mul-iter a b 0))
