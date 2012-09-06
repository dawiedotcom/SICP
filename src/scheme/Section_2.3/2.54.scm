; SICP Exercise 2.54
;   Dawie de Klerk
;   2012-09-05

(load "../utils.scm")

(define (equal? a b)
  (cond ((and (null? a)
              (null? b)) #t)
        ((or  (null? a)
              (null? b)) #f)
        ;((and (symbol? a) 
        ;      (symbol? b)
        ;      (eq? a b)) #t)
        ((and (pair? a) 
              (pair? b)
              (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))) #t)
        ((or (pair? a) 
             (pair? b)) #f)
        (else (eq? a b))))


(define (test)
  (print-eval (equal? '() '())) ; #t
  (print-eval (equal? '() 'a)) ; #f
  (print-eval (equal? '((x1 x2) (y1 y2)) '((x1 x2) (y1 y2)))) ; #t
  (print-eval (equal? '((x1 x2) (y1 y2)) '((x1 x2 x3) (y1 y2)))) ; #f
  (print-eval (equal? '(x1 x2) 'y1)) ; #f
  (print-eval (equal? 'abc 'abc)) ; #t
  (print-eval (equal? 123 123))); #t
