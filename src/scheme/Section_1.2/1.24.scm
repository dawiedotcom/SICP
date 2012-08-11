; SICP Exercise 1.24
; 	Dawie de Klerk
; 	2012-08-09

(load "1.22.scm")

(define (expmod base exp m)
  (cond ((zero? exp) 1)
		((even? exp)
		   (remainder (square (expmod base (/ exp 2) m)) m))
		(else
		   (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((zero? times) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (fast-prime n)
  (fast-prime? n 100))


(define (start-prime-test n start-time)
  (if (fast-prime n)
	  (begin 
		(report-prime n (- (runtime) start-time))
		#t)
      #f))

(println "Using fast-prime to test for primality.")
(search-for-primes 100000 3)
(search-for-primes 10000000 3)
(search-for-primes 1000000000 3)
