; SICP Exersice 3.18
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (cyclic? x)
  ;; Returns true if x contains any cycles that
  ;; would cause an infinite loop by cdr-ing through x.
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
  ;; A normal list with three symbols
  (print-eval (cyclic? '(a b c)))
  ;; The first pair points to the second and third
  (define x '(a b))
  (print-eval (cyclic? (cons x (cdr x))))
  ;; Each pair points to the next twice
  (define y '(a))
  (define z (cons y y))
  ;; A cyclic 2 element list
  (print-eval (cyclic? (cons z z)))
  (set-cdr! (cdr x) x)
  (print-eval (cyclic? x))
  ;; A cyclic 1 element list
  (define w (cons 'a 'b))
  (set-cdr! w w)
  (print-eval (cyclic? w))
  ;; A cyclic 2 element list where the 
  ;; last element points to itself
  (define s (list 'a 'b))
  (set-cdr! (cdr s) (cdr s))
  (print-eval (cyclic? s)))
