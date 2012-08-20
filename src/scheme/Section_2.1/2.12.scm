; SICP Exercise 2.12
;   Dawie de Klerk
;   2012-08-20

(load "2.10.scm")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.))

;; New constructors and selectors in term of the 
;; center and error percentage.
(define (make-center-percent c p)
  (let ((width (* c (/ p 100.))))
    (make-center-width c width)))
(define (percent i)
  (* (/ (width i) (center i)) 100.))

;; Test
(define (test-percent-constructor)
  (println "(make-center-percent 10 10)" (make-center-percent 10 10))
  (println "(make-center-percent 6.8 10)" (make-center-percent 6.8 10)))
(define (test-percent-selector)
  (println "(percent [9 11])" (percent (make-interval 9 11)))
  (println "(percent 6.8 10)" (percent (make-intreval 6.8 7.48))))
