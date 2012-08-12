; SICP Exercise 1.38
;   Dawie de Klerk
;   2012-08-12 
  
(load "../utils.scm")
(load "1.37.scm")

(define (tan-cf x k)
  ;; Calculates (tan x) using Lambert's continued
  ;; fraction.
  (define (n i)
    (if (= i 1) x 
        (- 0. (square x))))
  (define (d i)
     (- (* 2. i) 1.))
  (cont-frac n d k))

;;; Test agains the built in tan function
(define (test-tan-cf times)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.000001))
  (if (zero? times) 
      (println "Done")
      (let ((rand (/ (random 100) (random 10.))))
           (println (close-enough? (tan rand) (tan-cf rand 100))
                   "testing with" rand 
                   "Tan-cf" (tan-cf rand 100) 
                   "tan" (tan rand))
           (test-tan-cf (dec times)))))

(test-tan-cf 10)
