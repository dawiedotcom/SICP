; SICP Exercise 1.16
; 	Dawie de Klerk
; 	2012-08-06

(define (fast-exp b n)
  (define (fast-exp-iter b n a)
	(cond
	  ((zero? n) a)
	  ((zero? (remainder n 2))
	    (fast-exp-iter (* b b) (/ n 2) a))
	  (else
		(fast-exp-iter b (- n 1) (* a b)))))
  (fast-exp-iter b n 1))
