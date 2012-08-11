; SICP Exercise 1.28
; 	Dawie de Klerk
; 	2012-08-09

;(load "../utils.scm")
(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((zero? exp) 1)
		((even? exp)
		   (square-check (expmod base (/ exp 2) m) m))
		(else
		   (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square-check x n) 
  (if (non-trivial-root x n) 0 (remainder (square x) n)))

(define (non-trivial-root x n)
  (and 
	(not (or (= x 1) (= x (- n 1))))
	(= (remainder (square x) n) 1)))

(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))

(define (fast-prime? n)
  (define (fp a)
	(display a)
	(cond ((> a (/ n 2)) #t)
          ((miller-rabin-test n a) (fp (+ a 1)))
          (else #f)))
  (fp 1))

(define println
  ;; Print followed by a newline.
  (lambda args 
	(apply print args)
	(newline)))

(println "")
(println "Testing some composits")
(println (map fast-prime? '(10 100 4 6)))
(println "Testing some primes:")
(println (map fast-prime? '(2 3 5 13 19 23)))
(println "Testing the Carmicheal numbers")
(println "561 1105 1729 2465 2821 6601:")
(println (map fast-prime? '(561 1105 1729 2465 2821 6601)))
