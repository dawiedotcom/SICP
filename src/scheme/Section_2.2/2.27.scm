; SICP Exercise 2.27
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")


(define (deep-reverse lst)
  ;; Reverse the order of lst
  (define (iter acc t)
    (cond ((zero? (length t)) acc)
          ((pair? (car t))
            (iter (cons (deep-reverse (car t)) acc)
                  (cdr t)))
          (else
            (iter (cons (car t) acc) (cdr t)))))
  (iter '() lst))

(define (deep-reverse items)
  (fold (lambda (item acc)
          (if (pair? item)
              (cons (deep-reverse item) acc)
              (cons item acc)))
        '()
        items))
