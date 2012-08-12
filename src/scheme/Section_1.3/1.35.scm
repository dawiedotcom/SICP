; SICP Exercise 1.35
;   Dawie de Klerk
;   2012-08-12

(load "../utils.scm")

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;;; The quation x = 1 + 1/x can be maniplated 
;;;   x*x = x + 1
;;; to which the golden ratio is a solution.
(define phi
  ;; Calculate the golden ratio using the fixed-point
  ;; procedure.
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(println "The golden ratio:" phi)
