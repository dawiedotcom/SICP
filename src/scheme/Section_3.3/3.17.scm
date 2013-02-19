; SICP Exersice 3.17
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (count-pairs x)
  ;; Counts the number of unique pairs in x.
  (let ((counted '()))
    (define (iter x)
      (cond ((not (pair? x)) 0)
            ;((null? x) 1)
            ((pair? (memq x counted)) 0)
            (else
             (begin (set! counted (cons x counted))
                    (+ (iter (car x))
                       (iter (cdr x))
                       1)))))
    (iter x)))


(define (do-examples)
  (print-eval (count-pairs '(a b c)))
  (define x '(a b))
  (print-eval (count-pairs (cons x (cdr x))))

  (define y '(a))
  (define z (cons y y))
  (print-eval (count-pairs (cons z z)))
  (set-cdr! (cdr x) x)
  (print-eval (count-pairs x)))
