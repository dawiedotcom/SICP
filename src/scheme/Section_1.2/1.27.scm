; SICP Exercise 1.27
; 	Dawie de Klerk
; 	2012-08-09

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((zero? exp) 1)
		((even? exp)
		   (remainder (square (expmod base (/ exp 2) m)) m))
		(else
		   (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try a)
	(= (expmod a n n) a))
  ;(try-it (+ 1 (random (- n 1)))))
  (define (fermat-test-iter a)
	(cond ((= a n) #t)
		  ((try a) (fermat-test-iter (+ a 1)))
		  (else #f)))
  (fermat-test-iter 1))

(println "")
(println "Testing some composits")
(println (map fermat-test '(10 100 4 6)))
(println "Testing some primes:")
(println (map fermat-test '(2 3 5 13 19 23)))
(println "Testing the Carmicheal numbers")
(println "561 1105 1729 2465 2821 6601:")
(println (map fermat-test '(561 1105 1729 2465 2821 6601)))
