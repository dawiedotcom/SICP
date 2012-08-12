; SICP Exercise 1.38
;   Dawie de Klerk
;   2012-08-12

(load "../utils.scm")
(load "1.37.scm")

(define (d i)
  ;; Calculate the denomitator terms needed for
  ;; calculating the Euler's number by continued
  ;; fraction
  (let ((i+1 (inc i)))
       (if (zero? (remainder i+1 3))
           (* (/ i+1 3) 2)
           1)))


(define e 
    (+ 2 (cont-frac (lambda (i) 1.0)
                    d
                    100)))
      
(println "Euler's number calculated by continued fraction with 100 terms" e)
