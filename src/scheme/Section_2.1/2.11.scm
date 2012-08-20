; SICP Exercise 2.11
;   Dawie de Klerk
;   2012-08-19

(load "2.10.scm")

(define (mul-interval x y)
  (let ((xu (negative? (upper-bound x)))
        (xl (negative? (lower-bound x)))
        (yu (negative? (upper-bound y)))
        (yl (negative? (lower-bound y))))
  (cond ((or (not (and (xu xl yu yl))) (and xu xl yu yl))
         ; Everything is positive
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (xu xl yu yl))
         (make-interval 

