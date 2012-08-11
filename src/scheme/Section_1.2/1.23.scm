; SICP Exercise 1.23
; 	Dawie de Klerk
; 	2012-08-07

(load "1.21.scm")

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 3
	  (+ n 2)))
(define (timed-prime-test n)
  ;(print n)
  ;(display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
	  (begin 
		(report-prime n (- (runtime) start-time))
		#t)
      #f))
(define (report-prime n elapsed-time)
  (println n " *** " elapsed-time))

(define (search-for-primes n number)
  (if (zero? number) #t
	  (cond ((even? n) (search-for-primes (+ n 1) number))
	        ((timed-prime-test n) 
              (search-for-primes (+ n 2) (- number 1)))
	        (else 
		      (search-for-primes (+ n 2) number)))))

(println "Timed prime test, using naive prime testing and sequential searching with double step")
(search-for-primes 100000 3)
(search-for-primes 10000000 3)
(search-for-primes 1000000000 3)
