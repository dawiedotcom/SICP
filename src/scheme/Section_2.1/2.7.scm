; SICP Exercise 2.7
;   Dawie de Klerk
;   2012-08-18

(load "../utils.scm")

;;; The following procedures for interval arthimatic are given in
;;; the book.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;; Constructors and selectors

(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (cdr i))
(define (lower-bound i)
  (car i))
