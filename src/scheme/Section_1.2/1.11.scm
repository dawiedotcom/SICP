; SICP Exercise 1.11
; 	Dawie de Klerk (dawiedotcom@gmail.com)
; 	2012-08-04

(load "../utils.scm")

(define (f-rec n)
  (cond ((< n 3) n)
		(else (+
				(f-rec (- n 1))
				(* 2 (f-rec (- n 2)))
				(* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (f-iter- 2 1 0 n))

(define (f-iter- f1 f2 f3 count)
  (if (= count 0) f3
	  (f-iter- 
		(+ f1 (* 2 f2) (* 3 f3))
		f1
		f2
		(- count 1))))

(define (test-f n)
  ; Run both functions, printing their 
  ; results for comparison
  ;(display n)
  ;(display ": ")
  ;(display (f-iter n)) 
  ;(display " ")
  ;(display (f-rec n))
  ;(newline)
  (println n (f-iter n) (f-rec n)) 
  (if (> n 0) 
      (test-f (- n 1))))

(display "N Iter Rec")
(newline)
(test-f 10)
