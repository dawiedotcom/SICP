; SICP Section 3.5
;   Dawie de Klerk
;   2013-02-09

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

(define (take-stream s n)
  ;; Returns a stream containing the first n elements 
  ;; of a stream
  (if (zero? n)
      the-empty-stream
      (cons-stream (stream-car s) 
                   (take-stream (stream-cdr s) (- n 1)))))


;;; Exercise 3.54

(define (mul-streams s1 s2)
  ;; Produces the element wise product of two streams
  (stream-map * s1 s2))

(define factorials
  ;; Calculates the stream of factorials such that 
  ;; (stream-ref factorials n) is n!
  (cons-stream 1 (mul-streams integers factorials)))

;;; Exercise 3.55

(define (partial-sums s)
  ;; Calculates the partial sums of the stream s
  ;; i.e. (s0, s0+s1, s0+s1+s2, ...)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;;; Exercise 3.56

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;;; Exercise 3.59

(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series 
  (cons-stream 1 (stream-map (lambda (x) (- x))
                             (integrate-series sine-series))))

(define sine-series 
  (cons-stream 0 (integrate-series cosine-series)))

;;; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream 
    (* (stream-car s1) (stream-car s2))
    (add-streams  
      (scale-stream (stream-cdr s2) (stream-car s1))
      (mul-series (stream-cdr s1) s2))))

(define (do-3-60)
  (let ((one (add-streams (mul-series cosine-series cosine-series)
                          (mul-series sine-series sine-series))))
    (display-stream (take-stream one 10))))

;;; Exercise 3.61

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream 
                   (mul-series (stream-cdr s) 
                               (invert-unit-series s))
                   -1)))

(define (do-3-61)
  (let ((one (mul-series exp-series (invert-unit-series exp-series))))
    (display-stream (take-stream one 10))))

;;; Exercise 3.62

(define (div-series s1 s2)
  (let ((const (stream-car s2)))
  (if (zero? const)
      (error "DIV-SERIES: zero constant term" (take-stream s2 10))
      (scale-stream 
        (mul-series s1 
                    (invert-unit-series (scale-stream s2 (/ 1 const))))
        const))))

(define tangent-series
  (div-series sine-series
              cosine-series))

;;; Section 3.5.3

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1. n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequenc transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (do-examples-3-5-3)
  (print-eval (display-stream (take-stream (sqrt-stream 2) 10)))
  (print-eval (display-stream (take-stream pi-stream 10)))
  (print-eval (display-stream (take-stream (euler-transform pi-stream) 10)))
  (print-eval (display-stream (take-stream (accelerated-sequenc
                                             euler-transform
                                             pi-stream)
                                           10))))

;;; Exercise 3.64

(define (stream-limit s tolerance)
  (let ((n0 (stream-ref s 0))
        (n1 (stream-ref s 1)))
    (if (< (abs (- n0 n1)) tolerance)
        n1
        (stream-limit (stream-cdr s) tolerance))))


(define (sqrt-tol x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (do-3-64)
  (print-eval (sqrt-tol 2. 0.000001))
  (print-eval (stream-limit pi-stream 0.005))
  (print-eval (stream-limit (accelerated-sequenc
                              euler-transform
                              pi-stream) 0.000000001)))

;;; Exercise 3.65

(define ln2-stream
  (partial-sums (stream-map 
                  (lambda (x) 
                    (if (even? x)
                        (/ -1. x)
                        (/ 1. x)))
                  integers)))


(define (do-3-65)
  (println "ln2-stream")
  (display-stream (take-stream ln2-stream 10))
  (println "(euler-transform print-eval)")
  (display-stream (take-stream (euler-transform ln2-stream) 10))
  (println "(accelerated-sequenc euler-transform ln2-stream)")
  (display-stream (take-stream (accelerated-sequenc
                                             euler-transform
                                             ln2-stream)
                                           10)))

;;; Exercise 3.66

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s)
             (stream-cdr t)))))

(define (stream-zip s t)
  (if (or (stream-null? s)
          (stream-null? t))
      the-empty-stream
      (cons-stream (list (stream-car s)
                         (stream-car t))
                   (stream-zip
                     (stream-cdr s)
                     (stream-cdr t)))))

(define (do-3-66 n)
  (display-stream (take-stream 
                    (stream-zip integers (pairs integers integers))
                    n)))

;;; Exercise 3.73

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (rc r c dt)
  (lambda (i v0)
    (add-series 
      (integrate-series (scale-stream i c)
                        v0
                        dt)
      i)))
                
;;; Exercise 3.74

(define (sign-change-detector next last)
  (cond ((and (>= next 0) (< last 0)) 1)
        ((and (<= next 0) (> last 0)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream 
        (sign-change-detector (stream-car input-stream) last-value)
        (make-zero-crossings (stream-cdr input-stream)
                             (stream-car input-stream)))))

(define (zero-crossings-aph sense-data) (make-zero-crossings sense-data 0))

(define (zero-crossings sense-data)
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(define (do-3-74)
  (println "Alyssa")
  (display-stream (zero-crossings-aph (stream-enumerate-interval -3 3)))
  (println "Eva")
  (display-stream (zero-crossings (stream-enumerate-interval -3 3))))

;;; Exercise 3.75


(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;;; Exercise 3.76

(define (smooth input)
  (scale-stream (add-streams 
                  input-stream 
                  (stream-cdr input-stream))
                2))

(define (make-zero-crossings input-stream transform)
  (let ((smoothed-input (transform input-stream)))
    (stream-map
      sign-change-detector
      (cons-stream 0 smoothed-input)
      smoothed-input)))

(define (zero-crossings sense-data)
  (make-zero-crossings sense-data smooth))

;;; Exercise 3.77

;(define (integral delayed-integrand initial-value dt)
;  (define int
;    (cons-stream initial-value
;                 (let ((integrand (force delayed-integrand)))
;                   (add-streams (scale-stream integrand dt)
;                                int))))
;  int)

(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (if (stream-null? integrand)
                       the-empty-stream
                       (integral (delay (stream-cdr integrand))
                                 (+ (* dt (stream-car integrand))
                                    initial-value)
                                 dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (do-3-77)
  (println 
    (stream-ref (solve (lambda (y) y) 1 0.001) 1000)))

;;; Exercise 3.78

(define (solve-2nd y0 dy0 a b dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
                (scale-stream dy a)
                (scale-stream y b)))
  y)

(define (do-3-78)
  (println
    (stream-ref (solve-2nd 1 1 3 -2 0.001) 1000)))

;;; Exercise 3.79

(define (solve-2nd-gen f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (do-3-79)
  (println
    (stream-ref
      (solve-2nd-gen (lambda (dy y) ;dy)
                       (add-streams
                         (scale-stream dy 3)
                         (scale-stream y -2)))
                     1
                     1
                     0.001)
      1000)))
