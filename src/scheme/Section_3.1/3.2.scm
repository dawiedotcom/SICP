; SICP Exersice 3.2
;   Dawie de Klerk
;   2012-09-30

(define (make-monitored proc)
  (let ((counter 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) counter)
            ((eq? arg 'reset-count) (set! counter 0))
            (else
             (begin
               (set! counter (+ counter 1))
               (proc arg)))))))

(define (do-example)
  (let ((s (make-monitored sqrt)))
    (println "(define s (make-monitored sqrt))")
    (print-eval (s 100))
    (print-eval (s 'how-many-calls?))))

(define (test)
  (let ((sqrt (make-monitored sqrt))
        (square (make-monitored square)))
    (println "(define sqrt (make-monitored sqrt))")
    ;(println "(define square (make-monitored square))")
    (print-eval (sqrt 100))
    (print-eval (sqrt 144))
    (print-eval (sqrt 'how-many-calls?))
    (print-eval (sqrt 'reset-count))
    (print-eval (sqrt 'how-many-calls?))
    (print-eval (sqrt 100))
    (print-eval (sqrt 144))
    (print-eval (sqrt 'how-many-calls?))))
