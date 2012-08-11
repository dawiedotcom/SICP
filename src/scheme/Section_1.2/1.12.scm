; SICP Exercise 1.12
;	Dawie de Klerk
;	2012-08-05

(load "../utils.scm")

(define (snd ll)
  ;; Gets the second element in a list
  (car (cdr ll)))

(define (add-pairs lst)
  ;; Adds every two adjacent element in a list together
  (let ((len (length lst)))
    (cond 
      ((= len 0) '())
      ((= len 2) (list (apply + lst)))
      (else (cons (+ (car lst) (snd lst)) (add-pairs (cdr lst)))))))

(define (wrap-ones lst)
  ;; Adds a 1 at the begining and end of lst
  (append (list 1) lst (list 1)))

(define (pascal-triangle-row n)
  ;; Calculates the n-th row of Pascal's triangle.
  (cond 
	((= n 1) (list 1))
	((= n 2) (list 1 1))
	;(else (wrap-ones (add-pairs (pascal-triangle (- n 1)))))))
	(else (-> (- n 1) pascal-triangle-row add-pairs wrap-ones))))

(define (pascal-triangle n)
  ;; Prints the first n rows of Pascal's triangle.
  (define (pascal-triangle-helper n row)
    (if (> n row)
      (begin (println (pascal-triangle-row row))
	         (pascal-triangle-helper n (+ 1 row)))))
  (pascal-triangle-helper n 1))
  
