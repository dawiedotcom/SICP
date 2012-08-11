; SICP Exercise 1.22
; 	Dawie de Klerk
; 	2012-08-08

(load "../utils.scm")
(load "1.21.scm")


(define (prime? n)
  (= n (smallest-divisor n)))

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


(timed-prime-test 199)

(define (search-for-primes n number)
  (if (zero? number) #t
	  (cond ((even? n) (search-for-primes (+ n 1) number))
	        ((timed-prime-test n) 
              (search-for-primes (+ n 2) (- number 1)))
	        (else 
		      (search-for-primes (+ n 2) number)))))

(println "Timed prime test, using naive prime testing and sequential searching")
(search-for-primes 100000 3)
(search-for-primes 10000000 3)
(search-for-primes 1000000000 3)
