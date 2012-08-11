; SICP Exercise 1.6
(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-alyssa guess x)
  (new-if (good-enough? guess x)
	      guess
	      (sqrt-iter (improve guess x) x)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		   (else else-clause)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-sqrt x)
  (sqrt-iter 1.0 x))
(define (alyssa-sqrt x)
  (sqrt-iter-alyssa 1.0 x))

(display (new-sqrt 9))
(newline)
(display (alyssa-sqrt 9))
(newline)

; From
;  [1]: http://www.billthelizard.com/2009/10/sicp-exercises-16-18.html
;
;  	``The new-if procedure seems to work with simple conditional statements 
;  	on the command line, but if you then define the sqrt-iter procedure in 
;  	terms of new-if, you notice something peculiar. The sqrt-iter procedure 
;  	defined in terms of new-if never finishes executing.''
;
; But I did not see this happening...

