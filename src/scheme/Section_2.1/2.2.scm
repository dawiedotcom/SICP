; SICP Exercise 2.2
;   Dawie de Klerk
;   2012-08-17

(load "../utils.scm")

;;; Points constructors and selectors.

(define (make-point x y)
  ;; Represent a point as the pair ( x . y )
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

;;; Line segments constructors and selectors.

(define (make-segment start end)
  ;; Represent a line as a pair of points.
  (cons start end))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

;;; Lines and points in term of constructors and selectors.

(define (midpoint segment)
  ;; Calculates the midpoint of a line segment.
  (make-point 
    (/ (+ (x-point (start-segment segment))
          (x-point (end-segment segment))) 2)
    (/ (+ (y-point (start-segment segment))
          (y-point (end-segment segment))) 2)))

(define (segment-length segment)
  ;; Calculate the length of a line segment
  (sqrt (+ (square (- (x-point (start-segment segment)) 
                      (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment)) 
                      (y-point (end-segment segment)))))))

;;; Test

(define (point->string p)
  (string-append "(" 
                 (number->string (x-point p)) "," 
                 (number->string (y-point p) ")")))

(define (print-point p)
  (display (point->string p)))

(define (test-midpoint)
  (define (test-one p1 p2)
    (println "Midpoint between "
             (point->string p1) "and"
             (point->string p2) "is"
             (point->string (midpoint (make-segment p1 p2)))))
  (let ((p01 (make-point 0 1))
        (p10 (make-point 1 0))
        (p-11(make-point -1 1))
        (p11 (make-point 1 1)))
    (carthesian-map test-one 
                    (list p01 p10 p-11 p11) 
                    (list p01 p10 p-11 p11))))
