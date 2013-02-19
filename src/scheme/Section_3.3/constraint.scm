; SICP Section 3.3.5
;   Dawie de Klerk
;   2012-12-17

(load "../utils.scm")

;;;; Basic operations on connectors

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "CONNECTOR: Unknown operation" request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else
           (procedure (car items))
           (loop (cdr items)))))
  (loop list))

;;;; Constraints

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                      (- (get-value sum)
                         (get-value a2))
                      me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "ADDER: Unknown request" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "CONSTANT: Unknown request" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
  
(define (probe name connector)
  (define (print-probe value)
    (println "Probe:" name "=" value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "PROBE: Unknown request" request))))
  (connect connector me)
  me)
               
(define (squarer a b)
  ;; Exercise 3.35: Squarer as a primative constraint.
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "SQUARER: square is less than 0" (get-value b))
               (set-value! a
                           (sqrt (get-value b))
                           me)))
          ((has-value? a)
           (set-value! b
                       (square (get-value a))
                       me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "SQUARER: Unknown request" request))))
  (connect a me)
  (connect b me)
  me)

;;;; Exercises and examples

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (averager a b c)
  ;;; Exercise 3.33: Establishes the constraint that c is the 
  ;;; average of a and b.
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (multiplier x y c)
    (constant 0.5 y)
    'ok))

(define (temperature-example)
  (define C (make-connector))
  (define F (make-connector))
  (celsius-fahrenheit-converter C F)

  (probe "Culsius temp" C)
  (probe "Fahrenheit temp" F)
  (set-value! C 25 'user)
  (forget-value! C 'user)
  (set-value! F 212 'user))

(define (averager-test)
  (let ((a (make-connector))
        (b (make-connector))
        (c (make-connector)))
    (averager a b c)
    (probe "a" a)
    (probe "b" b)
    (probe "c" c)

    (set-value! a 1 'user)
    (set-value! b 3 'user)

    (forget-value! b 'user)
    (set-value! c 2 'user)))

(define (ex-3-34)
  (define (squarer a b)
    (multiplier a a b))
  (let ((a (make-connector))
        (b (make-connector)))
    (squarer a b)
    (probe "a" a)
    (probe "b" b)
    (set-value! a 2 'user)
    (forget-value! a 'user)
    ;; Setting b won't work because multiplier can only calulate the
    ;; value of one terminal if two others are given so after setting 
    ;; b, it will wait until one of the factors is asigned a value too.
    (set-value! b 4 'user)))

(define (ex-3-35)
  (let ((a (make-connector))
        (b (make-connector)))
    (squarer a b)
    (probe "a" a)
    (probe "b" b)
    (set-value! a 2 'user)
    (forget-value! a 'user)
    (set-value! b 4 'user)))


;;; Exercise 3.37

;(define (c+ x y)
;  (let ((z (make-connector)))
;    (adder x y z)
;    z))

(define (divider a b c) (multiplier b c a))

(define (expressive constraint . connectors)
  ;; A helper function to make a new constraint between connectors
  ;; and returns the newly created connector.
  (let ((result (make-connector)))
    (apply constraint 
           (append connectors (list result)))
    result))

;; A simple syntax interface for constraints.
(define (c+ x y) (expressive adder x y))
(define (c* x y) (expressive multiplier x y))
(define (c/ x y) (expressive divider x y))
(define (cv x) (expressive constant x))

(define (ex-3-37)
  ;; Do the example in the text
  (define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))
  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))
  (probe "Culsius temp" C)
  (probe "Fahrenheit temp" F)
  (set-value! C 25 'user)
  (forget-value! C 'user)
  (set-value! F 212 'user))
