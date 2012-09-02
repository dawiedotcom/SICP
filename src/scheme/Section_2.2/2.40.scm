; SICP Exercise 2.40
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")

(define (enumerate-interval low high)
  (if (> low high) '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  ;; Recursive definition, which is a bit clearer IMO
  (if (zero? n) '()
      (append 
        (map (lambda (a) (list n a)) (enumerate-interval 1 (+ n -1)))
        (unique-pairs (+ n -1)))))

(define (unique-pairs n)
  ;; In terms of flatmap/append-map. Probably 
  ;; the answer the book is looking for.
  (append-map 
    (lambda (a) 
      (map (lambda (b) (list a b))
           (enumerate-interval 1 (+ a -1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  ;; The new prime-sum-pairs in terms of unique pairs
  (map make-pair-sum
       (filter prime-sum? unique-pairs)))
