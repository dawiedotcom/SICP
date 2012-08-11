; SICP Exercise 1.8
(define (square x)
  (* x x))
(define (cube x)
  (* (square x) x))

(define (cube-rt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (cube-rt-iter (improve guess x) x)))

(define (improve guess x)
  (/ 
	(+ 
	  (/ x (square guess))
	  (* 2 guess))
	3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 
	 0.001))

(define (cube-rt x)
  (cube-rt-iter 1.0 x))

(display (cube-rt 27))
(newline)
(display (cube-rt 64))
(newline)
