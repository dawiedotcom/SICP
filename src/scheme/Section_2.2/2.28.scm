; SICP Exercise 2.27
;   Dawie de Klerk
;   2012-08-25

(define (fringe tree)
  (fold (lambda (item acc)
          (if (pair? item)
              (append acc (fringe item))
              (append acc (list item))))
        '()
        tree))
