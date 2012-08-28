; SICP Exercise 2.23
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (for-each action items)
  (define (iter things)
    (if (not (null? things))
      (begin (action (car things))
             (iter (cdr things)))))
  (iter items))

(define (for-each action items)
  (begin (map action items)
         #t))
