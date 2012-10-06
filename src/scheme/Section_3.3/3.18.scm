; SICP Exersice 3.18
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (cyclic? x)
  ;; Counts the number of unique pairs in x.
  (let ((visited '()))
    (define (iter x)
      (cond ((null? (cdr x)) #f)
            ((and (pair? (memq x visited))
                  (pair? (memq (cdr x) visited))) #t)
            (else
             (begin (set! visited (cons x visited))
                    (iter (cdr x))))))
    (iter x)))


(define (do-examples)
  (print-eval (cyclic? '(a b c)))
  (define x '(a b))
  (print-eval (cyclic? (cons x (cdr x))))

  (define y '(a))
  (define z (cons y y))
  (print-eval (cyclic? (cons z z)))
  (set-cdr! (cdr x) x)
  (print-eval (cyclic? x)))
