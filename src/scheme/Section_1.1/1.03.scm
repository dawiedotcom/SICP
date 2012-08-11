; sicp 1.3

(define (middle a b c)
  (max
    (min a b)
    (min b c)))
    
(define (sum-squares-larger a b c)
  (let ((x (max a b c))
        (y (middle a b c)))
    (+ (* x x) (* y y))))

