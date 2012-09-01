; SICP Exercise 2.37
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")
(load "2.36.scm")


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (test)
  (let ((rot-90 '((0 1) (-1 0)))
        (e1 '(1 0))
        (e2 '(0 1)))
    (print-eval rot-90)
    (print-eval e1)
    (print-eval e2)

    (print-eval (dot-product e1 e2))
    (print-eval (transpose rot-90))

    (print-eval (matrix-*-vector rot-90 e1))
    (print-eval (matrix-*-vector rot-90 e2))

    (print-eval (matrix-*-vector (transpose rot-90) e1))
    (print-eval (matrix-*-vector (transpose rot-90) e2))
    
    (print-eval (matrix-*-matrix rot-90 rot-90))
    (print-eval (matrix-*-matrix rot-90 (transpose rot-90)))))
