; SICP Exercise 2.21
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map square items))
