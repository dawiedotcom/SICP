; SICP Exercise 2.41
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")
(load "2.40.scm")

(define (triples-less-than-n-that-sum-to-s n s) ;P
  (define (sum-to-s? triple)
    (= (apply + triple) s))
  (filter sum-to-s? (unique-triples n)))

(define (unique-triples n)
  (append-map
    (lambda (unique-pair)
      (map (lambda (k) (append unique-pair (list k)))
           (enumerate-interval 1 (+ (apply min unique-pair) -1))))
    (unique-pairs n)))
