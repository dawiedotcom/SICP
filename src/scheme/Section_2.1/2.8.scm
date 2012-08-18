; SICP Exercise 2.8
;   Dawie de Klerk
;   2012-08-18

(load "2.7.scm")

;;; The difference between two intervals is the interval
;;; addition of the first interval with the negative of the
;;; upper-bound and negative of the lower bound of the second
;;; interval, i.e, [a,b] - [c,d] = [a-d,b-c]

(define (sub-interval x y)
  (add-interval x
				(make-interval (- 0.0 (upper-bound y))
							   (- 0.0 (lower-bound y)))))
