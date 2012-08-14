; SICP Exercise 1.40
;   Dawie de Klerk
;   2012-08-13

(load "1.36.scm")

(define (newton-transform g)
  ;; Calculates the fixed point function for 
  ;; Newton's method
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  ;; Finds zeros of g, using Newton's method and
  ;; an initial guess.
  (fixed-point (newton-transform g) guess))
(define (deriv g)
  ;; Returns a procedure that approximates the
  ;; dirivative of g.
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cubic a b c)
  ;; Returns a procedure that caluclates the cubic
  ;; polinomial x^3 + a.x^2 + b.x + c
  (define (cube x) 
    (* x x x))
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (test-cubic)
  ;; Test the cubic function by finding roots
  ;; to known polynomials with Newton's method
  (println "x^3=0, when x=" 
           (newtons-method (cubic 0. 0. 0.) 1))
  (println "x^3 + 1 =0, when x=" 
           (newtons-method (cubic 0. 0. 1.) 1))
  (println "x^3 - x =0, when x=" 
           (map (lambda (x) 
                  (newtons-method (cubic 0. -1. 0.) x))
                '(-10. 0.24 10))))
