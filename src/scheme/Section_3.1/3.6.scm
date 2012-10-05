; SICP Exersice 3.6
;   Dawie de Klerk
;   2012-10-05

(load "../utils.scm")

(define rand-update 
  ;; A simple implementation of rand-update using a LCG
  (let ;((a 1664525)
       ; (c 1013904223)
       ; (m (expt 2 32)))
       ((a 15)
        (c 1)
        (m 32))
    (lambda (x)
      (remainder (+ (* x a) c) m))))


(define rand
  (let ((x 0))
    (lambda (proc)
      (cond ((eq? proc 'generate)
             ;; Generate a new random number based on
             ;; the result of the last one
             (begin
               (set! x (rand-update x))
               x))
            ((eq? proc 'reset)
             ;; Reset the seed value.
             (lambda (new-x)
               (set! x new-x)))
            (else
              (error "RAND - Unknown instruction" proc))))))
           
(define (do-examples)
  (print-eval ((rand 'reset) 10))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate))
  (print-eval ((rand 'reset) 10))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate))
  (print-eval (rand 'generate)))
