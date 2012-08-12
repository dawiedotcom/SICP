; SICP Exercise 1.37
;   Dawie de Klerk
;   2012-08-12

(load "../utils.scm")

(define (cont-frac n d k)
  ;; Calculate the k-term continued fraction using
  ;; n and d procedures to calculate the k-th terms.
  (define (iter i result)
    (if (zero? i) result
        (let ((d-i (d i))
			  (n-i (n i)))
             (iter (- i 1) (/ n-i (+ d-i result))))))
  (iter k 0))

(define (phi k)
  ;; Calculate the golden ratio using cont-frac
  (/ 1.0 (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k)))

;;; Test with some known output
(println "Calulating the golden ratio using cont-frac"
         (phi 100))

(define (phi-accuracy-test)
  ;; Test phi with increasing input until the golden ratio
  ;; is calculated te 4 significant digits.
  (define (iter k result)
    (println "Result so far" result "with" k "iterations")
    (if (> (abs (- result 1.6180))
           0.0001)
        (iter (+ k 1) (phi (+ k 1)))))
  (iter 1 0))

;(phi-accuracy-test)
