; SICP Exercise 2.20
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (same-parity x . xs)
  ;; Same parity using filter
  (cons x
    (filter (lambda (y) (= (remainder x 2) 
                           (remainder y 2)))
            xs)))

(define (same-parity2 x . xs)
  ;; Same parity with recursion
  (define (iter acc lst)
    (cond ((zero? (length lst)) acc)
          ((= (remainder x 2) (remainder (car lst) 2))
            (iter (append acc (list (car lst))) (cdr lst)))
          (else 
            (iter acc (cdr lst)))))
  (iter (list x) xs))

(define (test-same-parity)
  (println "(same-parity 1 2 3 4 5 6 7 8 9)"
            (same-parity 1 2 3 4 5 6 7 8 9))
  (println "(same-parity 2 3 4 5 6 7 8 9)"
            (same-parity 2 3 4 5 6 7 8 9))
  (println "(same-parity2 1 2 3 4 5 6 7 8 9)"
            (same-parity2 1 2 3 4 5 6 7 8 9))
  (println "(same-parity2 2 3 4 5 6 7 8 9)"
            (same-parity2 2 3 4 5 6 7 8 9)))
