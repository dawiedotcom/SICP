; SICP Section 3.5
;   Dawie de Klerk
;   2012-02-09

(load "../utils.scm")

;;; Section 3.51

(define the-empty-stream '())
(define stream-null? null?)
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
;(define (cons-stream a b) (cons a (delay b)))
;
(define-syntax cons-stream
  ;; A function 
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;(define-macro (cons-stream a b)
;  `(cons ,a (delay ,b)))

;(delay 

(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream
        a
        (stream-enumerate-interval (+ a 1) b))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) 
                  (- n 1))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (display x)
  (newline))

;; Exercise 3.50

(define (stream-map proc . argstreams)
  ;; Maps proc over any number of lists.
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (do-3-50)
  (print-eval 
    (display-stream
      (stream-map 
        +
        (stream-enumerate-interval 0 5)
        (stream-enumerate-interval 10 15)))))

;; Exercise 3.51

(define (show x)
  (display-line x)
  x)


(define (do-3-51)
  (let ((x (stream-map show (stream-enumerate-interval 0 10))))
    (print-eval (stream-ref x 5))
    (print-eval (stream-ref x 7))))

;; Exercise 3.52

(define (do-3-52)
  ;; Stream-map and stream-filter reverses the order of the streams.
  ;; Seq is a stream representation of (20 39 57 ... 210), because
  ;; 20 19 18 .. 1 is added to sum in that order. 
  ;;
  ;; y is a stream representation of (210 204 200 182 174 144 132 90 74 20),
  ;; all the even values in seq, reversed. 
  ;; z is a stream representation of (210 200 195 165 155 105 90 20)
  (let ((sum 0))
    (define (accum x)
      (set! sum (+ x sum))
      (println "ACCUM - sum=" sum)
      sum)
    (let* ((seq (stream-map accum (stream-enumerate-interval 1 20)))
           (y (stream-filter even? seq))
           (z (stream-filter (lambda (x) (= (remainder x 5) 0))
                             seq)))
      (print-eval (stream-ref y 7))
      (display-stream z))))

;;; Section 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (zero? (remainder x y)))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream 0
               (cons-stream 1 
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;;; Exercise 3.54

(define (mul-streams s1 s2)
  ;; Produces the element wise product of two streams
  (stream-map * s1 s2))

(define factorials
  ;; Calculates the stream of factorials such that 
  ;; (stream-ref factorials n) is n!
  (cons-stream 1 (mul-streams integers factorials)))
