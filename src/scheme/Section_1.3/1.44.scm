; SICP Exercise 1.42
;   Dawie de Klerk
;   2012-08-13

(load "../utils.scm")
(load "1.43.scm")

(define (smooth f dx)
  ;; Returns the smoothing of f 
  (lambda (x)
    (average 
      (f (+ x dx))
      (f (- x dx))
      (f x))))
(define average 
  ;; Returns the average of the numeric arguments.
  (lambda args
    (if (> (length args) 0)
        (/ (fold + (car args) (cdr args))
           (length args)))))

(define (nth-smooth f n dx)
  ;; Returns the n-fold smoothed function of f.
  (repeated (smooth f dx) n))

(define (test-smooth)
  ;; Test smooth with cosine around 0. 
  (println "Smoothed cosine at x=0, dx=0.1"
           ((smooth cos 0.1) 0.))
  (println "Smoothed cosine at x=0, dx=0.01"
           ((smooth cos 0.01) 0.))
  (println "Smoothed cosine at x=0, dx=0.5"
           ((smooth cos 0.5) 0.))
  (println "3-fold smooth of cosine at x=0, dx=0.01"
           ((nth-smooth cos 3 0.01) 0.)))
