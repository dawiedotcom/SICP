; SICP Exercise 1.36
;   Dawie de Klerk
;   2012-08-12

(load "../utils.scm")

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
	  ;(println "Gussing" next) ; Print the next guess
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (test-fixed-point)
  (println "Finding the fixed point of x^x = 1000:")
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5))
