; SICP Exercise 2.14
;   Dawie de Klerk
;   2012-08-20

(load "2.12.scm")

(define (test-div)
  (define (test c1 p1 c2 p2)
    (let ((i1 (make-center-percent c1 p1))
          (i2 (make-center-percent c2 p2)))
      (println c1 "+-" p1 "/" c2 "+-" p2 "=" 
               (center (div-interval i1 i2)) "+-"
               (percent (div-interval i1 i2)))))
  (test 100 10 100 10)
  (test 100 1 100 1)
  (test 200 1 100 1)
  (test 100 1 200 1))

