; SICP Exercise 1.45
;   Dawie de Klerk
;   2012-08-13

(load "../utils.scm")
(load "1.36.scm")     ; fixed-point
(load "1.43.scm")     ; repeated
(load "1.44.scm")     ; average

(define (average-damp f)
  ;; Retruns a procedure that calculates the 
  ;; average damping of f
  (lambda (x)
    (average x (f x))))

; Some experements revealed the following:
; ROOT    n   # AVERAGE NEEDED
; second  2   1x
; third   3   1x
; fourth  4   2x average
; fith    5   2x
; sixth   6   2x
; seventh 7   2x
; eigth   8   3x
; which sugests we need (log n)-fold repeated average.

(define (nth-root x n)
  (define (log2 a)
    (truncate (/ (log a) (log 2))))
  (fixed-point 
    ((repeated average-damp (log2 n))
        (lambda (y) (/ x (expt y (dec n)))))
    1.0))

(define (test-nth-root)
  (define (test base exp)
    (println exp "th-root of " base "^" exp
             (nth-root (expt base exp) exp)))
  (carthesian-map test '(2 3 4 5 6) '(2 3 4 5 6)))
