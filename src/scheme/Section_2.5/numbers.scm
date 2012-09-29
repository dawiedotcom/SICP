; SICP Section 2.5.1
;   Dawie de Klerk
;   2012-09-29

(load "../utils.scm")

;;; Exercise 2.77

(define z (make-complex-from-real-imag 3 4))
;=> ('complex 'rectangular (3 . 4))

;   Louis' original procedure call can be traced as follows
; (magnitude z)
; (magnitude ('complex 'rectangular (3 . 4)))
; (apply-generic 'magnitude ('complex 'rectangular (3 . 4)))
;   Here it becomes a bit tricky, but the second let statement in
;   apply-generic will evaluate:
; (get 'magnitude 'complex) 
;   This is where the error occurs because none of the packages
;   adds an operation magnitude for data taged with data complex.
;   They only 'put' operations for add, sum, mul and div.
;
;   After Alyssa's fix, the following will be evaluated:
; (magnitude z)
; (magnitude ('complex 'rectangular (3 . 4)))
; proc -> (get 'magnitude 'complex)
;         magnitude
; (apply magnitude ('rectangular (3 . 4)))
;   The call to maginuted here is the one defined in terms of
;   apply-generic in the top level

; (apply-generic 'magnitude ('ractangular (3 . 4)))
; proc -> (get 'magnitude 'rectangular)
; (apply magnitude (3 . 4))        
;   This magnitude is the one defined in the rectangular package
; (sqrt (+ (square 3) (square 4)))
; 5


;;; Exercise 2.78


;; The original definitions for type-tag, contents and attach-tag
;; was

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; To use scheme's representation for numbers, we can modify them
;; to use cons to check for a special case where the arguments are
;; numbers. 

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cons ((pair? datum) (car datum))
        ((number? datum) datum)
        (else
          (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cons (pair? datum) (cdr datum)
        ((number? datum) datum)
        (else
          (error "Bad tagged datum -- CONTENTS" datum))))


;;; Exercise 2.79

(define (equ? x y)
  (apply-generic 'equ x y))

;; In install-scheme-number-package
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

;; In install-rational-package
  (put 'equ '(rational rational)
       (lambda (x y)
         (and (= (denom x) (denom y))
              (= (numer x) (numer y)))))

;; In install-complex-package
  (put 'equ '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))

;;; Exercise 2.80

(define (=zero? x)
  (apply-generic '=zero x))

;; In install-scheme-number-package
  (put '=zero '(scheme-number scheme-number)
       (lambda (x) (= x 0)))

;; In install-rational-package
  (put '=zero '(rational rational)
       (lambda (x)
         (= (numer x) 0)))

;; In install-complex-package
  (put '=zero '(complex complex)
       (lambda (x)
         (and (= (real-part x) 0)
              (= (imag-part x) 0))))
