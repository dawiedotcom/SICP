; SICP Exercise 2.10
;   Dawie de Klerk
;   2012-08-19

(load "2.8.scm")

(define (div-interval x y)
  ;; Same as div-interval given in the text, but 
  ;; signals an error when dividing by an interval spanning zero.
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
    (error "Invalid division by interval spanning zero")
    (mul-interval x 
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))
