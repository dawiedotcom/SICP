; SICP Exercise 2.53
;   Dawie de Klerk
;   2012-09-05

(load "../utils.scm")

(define (run)
  (print-eval (list 'a 'b 'c))
  (print-eval (list (list 'george)))
  (print-eval (cdr '((x1 x2) (y1 y2))))
  (print-eval (cadr '((x1 x2) (y1 y2))))
  (print-eval (pair? (car '(a short list))))
  (print-eval (memq 'red '((red shoes) (blue socks))))
  (print-eval (memq 'red '(red shoes blue socks))))
