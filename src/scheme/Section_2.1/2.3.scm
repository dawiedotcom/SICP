; SICP Exercise 2.3
;   Dawie de Klerk
;   2012-08-17

(load "2.2.scm")
(load "../utils.scm")
;;; Define properties of rectangles in terms of selectors. 

(define (rect-area rect)
  (* (rect-width rect) 
     (rect-height rect)))

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) 
          (rect-height rect))))

;;; Something to test with
(define (rect->string rect)
  (string-append 
    (number->string (rect-width rect))
    "x"
    (number->string (rect-height rect))))

(define (test-rect)
  (define (test-one rect)
    (println "Rectangle"
             (rect->string rect)
             ": area="
             (rect-area rect)
             ", perimeter"
             (rect-perimeter rect)))
  (let ((r1 (make-rect (make-point -1 -1)
                       (make-point 1 1)))
        (r2 (make-rect (make-point 0 0)
                       (make-point 0.5 10))))
    (test-one r1)
    (test-one r2)))

;;; Represeting rectangles as a pair of points.
(define (make-rect bottom-left top-right)
  (cons top-right bottom-left))

(define (rect-width rect)
  (abs (- (x-point (car rect))
          (x-point (cdr rect)))))
(define (rect-height rect)
  (abs (- (y-point (car rect))
          (y-point (cdr rect)))))

(println "Testing rep with points:")
(test-rect)

;;; Representing rectangles as a pair of lines
(define (make-rect top-right bottom-left)
  (let ((top-left (make-point (x-point bottom-left)
                              (y-point top-right))))
    (cons (make-segment top-left top-right)
          (make-segment bottom-left top-left))))

(define (rect-width rect)
  (segment-length (car rect)))
(define (rect-height rect)
  (segment-length (cdr rect)))

(println "Testing rep with lines")
(test-rect)
