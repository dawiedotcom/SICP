; SICP Exercise 2.9
;   Dawie de Klerk
;   2012-08-18

(load "2.8.scm")

;; Define some intervals
(define one-two 
  (make-interval 1 2))
(define one-four
  (make-interval 1 4))

(define ten-eleven
  (make-interval 10 11))
(define ten-thirteen
  (make-interval 10 13))

(define test-intervals
  (list one-two one-four ten-eleven ten-thirteen))

;; The width of the intervals
(define (interval-width i)
  (- (upper-bound i) (lower-bound i)))
(define (interval->string i)
  (string-append "[" 
                 (number->string (lower-bound i))
                 ","
                 (number->string (upper-bound i))
                 "]"))

;; Some tests of the width of intervals after adding
(define (test-width-add)
  (define (test a b)
    (= (interval-width (add-interval a b))
       (+ (interval-width a)
          (interval-width b))))
  (and (carthesian-map test test-intervals test-intervals)))

(define (test-width-mul)
  (define (print-mul a b)
    (println "w(" (interval->string a) ") ="
             (interval-width a)
             "; w(" (interval->string b) ") ="
             (interval-width b)
             "; w(" (interval->string a) "*" (interval->string b) ") ="
             (interval-width (mul-interval a b))))
  (print-mul one-two one-four)
  (print-mul ten-eleven ten-thirteen))
