; SICP Exercise 1.25
; 	Dawie de Klerk
; 	2012-08-09

(load "1.24.scm")

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(println "Using expmod with fast-expt")
;; NOTE this evaluates the eponent before the 
;; modulo, meaning there are very big numbers 
;; involved in the computation! See
;; 	[1]: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#footnote_Temp_78
(search-for-primes 100000 3)
(search-for-primes 10000000 3)
(search-for-primes 1000000000 3)
