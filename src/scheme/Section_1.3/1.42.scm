; SICP Exercise 1.42
;   Dawie de Klerk
;   2012-08-13

(load "../utils.scm")

(define (compose f g)
  ;; Returns the composition f(g(x))
  (lambda (x)
    (f (g x))))

(define (test-compose)
  (println "((compose square inc) 6)"
           ((compose square inc) 6)))
