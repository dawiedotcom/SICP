; SICP Exersice 3.8
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define f
  ;; If x is a number, return the first 
  ;; numeric argument f was called with
  ;; since it was reset.
  (let ((first-called-with 'nan))
    (lambda (x)
      (if (number? x)
             (begin 
               (if (not (number? first-called-with))
                   (set! first-called-with x))
               first-called-with)
             (set! first-called-with 'nan)))))

(define (do-example)
  (print-eval (+ (f 0) (f 1)))
  (print-eval (f 'reset))
  (print-eval (+ (f 1) (f 0))))
