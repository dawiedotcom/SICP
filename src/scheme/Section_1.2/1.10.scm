; SICP Exercise 1.10
; 	Dawie de Klerk (dawiedotcom@gmail.com)
;	2012-08-04


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(display "(A 1 10) = ")
	(display (A 1 10)) 
	(newline)
(display "(A 2 4) = ")
	(display (A 2 4)) 
	(newline)
(display "(A 3 3) = ")
	(display (A 3 3)) 
	(newline)

(define (f n) (A 0 n))
; Same as 2*n
; (A 0 n)
; (* 2 y)
; because the second condition (= x 0) is always true.

(define (g n) (A 1 n))
; Same as 
; 	g(0) = 0
; 	g(n) = 2^n:
; (A 1 n)
; (A 0 (A 1 (- n 1)))
; (A 0 ([... n-1 times ...] 2))
; (A 0 ([...n-2 times] 4)

(define (h n) (A 2 n))
; Same as
; 	h(0) = 0
; 	h(n) = 2^(2^n)
; or just 
; 	h(n) = 2^g(n)
; (A 2 n)
; (A 1 (A 1 (- n 1)))
; (A 1 (^ 2 n))
; From which we recognize the first step of g

(define (k n) (* 5 n n))
; 5 n^2
