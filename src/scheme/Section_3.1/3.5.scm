; SICP Exersice 3.5
;   Dawie de Klerk
;   2012-09-30

(load "../utils.scm")


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((area 
          (* (- x2 x1)
             (- y2 y1)))
        (experiment
          (lambda ()
            (p (random-in-range x1 x2) (random-in-range y1 y2)))))
    (* (monte-carlo trials experiment)
       area)))

(define (test-s1)
  (define (as-fraction trials)
    (let ((pi 3.14159265359))
      (/ (estimate-integral (lambda (x y) 
                              (< (sqrt (+ (square x) (square y))) 1))
                            -1. 1.
                            -1. 1.
                            trials)
         pi)))
  (println "With 100 trials" (as-fraction 100))
  (println "With 300 trials" (as-fraction 300))
  (println "With 1000 trials" (as-fraction 1000))
  (println "With 3000 trials" (as-fraction 3000))
  (println "With 10000 trials" (as-fraction 10000))
  (println "With 30000 trials" (as-fraction 30000))
  (println "With 100000 trials" (as-fraction 100000)))
