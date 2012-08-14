; SICP Exercise 1.46
;   Dawie de Klerk
;   2012-08-14

(load "../utils.scm")
(load "1.44.scm")

(define (iterative-improve good-enough? improve)
  ;; Returns a procedure that performs a numeric 
  ;; calculation by iterative improvement, starting
  ;; from an intial guess.
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (lambda (g)
    (try g)))

(define (fixed-point f first-guess)
  ;; Calculates the fixed point of f
  ;; using iterative improvement.
  ((iterative-improve
     (lambda (guess) 
       ; Test a guess by comparing against (f guess)
       (< (abs (- guess (f guess))) 0.0001))
     f); The function is used to improve a guess. 
   first-guess))

(define (sqrt x)
  ;; Calculates square roots using iterative
  ;; improvement
  ((iterative-improve
    (lambda (guess)
      ; Test a guess by comparing its square to
      ; sqrt's input
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      ; Improve a guess using Newton's method.
      (average guess (/ x guess))))
  1.0))

(define (test-sqrt)
  (println "Sqrt of 4:" (sqrt 4.)) 
  (println "Sqrt of 49:" (sqrt 49.)) 
  (println "Sqrt of 64:" (sqrt 64.)) 
  (println "Sqrt of 144:" (sqrt 144.)))
(define (test-fixed-point)
  (println "Finding the fixed point of x^x = 1000:")
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5))
