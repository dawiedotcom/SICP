; SICP Section 3.3.4
;   Dawie de Klerk
;   2012-12-15

(load "../utils.scm")
(load "queue.scm")

;;;;
;;;; Wires

(define (make-wire)
  ;;; Creates a new wire. Wires have two state variables, a 
  ;;; signal-value (0 or 1) and a list of procedures that 
  ;;; is invoked when the signal-value changes.
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      ;; Updates the signal value and calls each action if
      ;; the value changes
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      ;; Adds a new action procedure to the list and invokes
      ;; the new procedure.
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "WIRE: Unknown operation" m))))
    dispatch))

;;;; Wrapper procedures for a wire
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

;;;;
;;;; The agenda

(define (make-time-segment time queue)
  ;;; Constructs a time segment - a time stamp and a 
  ;;; queue of actions that should be performed at that
  ;;; time.
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))



(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  ;;; Adds a new action to the the agenda at a given time
  (define (belongs-before? segments)
    ;; Should action be added before the list of segments.
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    ;; Makes a new segment with a given time and one action.
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    ;; Adds a new segment to the agenda, keeping the segments 
    ;; sorted by their time stamps
    (if (= (segment-time (car segments)) time)
        ; Add the action to the current segment.
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              ; Add a new segment before the curretn segment
              (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
              ; Add after the current segement.
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        ; Add a new segment at the beginning of the agenda.
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        ; Add a new segment after the biginning. 
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  ;;; Removes the first action from the agenda, deleting a 
  ;;; time segement if neccessary.
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  ;;; Gets the first action in the agenda.
  (if (empty-agenda? agenda)
      (error "FIRST-AGENDA-ITEM: Agenda is empty" agenda)
      (let ((first-seg (first-segment agenda)))
        ;(println "Updating agenda time" (segment-time first-seg))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;;; Logic function

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 1) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 0)
        ((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signal" s))))

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 1) (= b 0)) 1)
        ((and (= a 0) (= b 1)) 1)
        ((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signal" s))))

;;;; Primative components

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1)
                         (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  ;;; Exercise 3.28: Makes an or gate, similar to an and gate
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1)
                        (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;;; Compound components

(define (or-gate-compound a1 a2 output)
  ;;; Exercise 3.29: An or gate constructed using and-gate and
  ;;;   inverters.
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    ;(probe 'd d)
    ;(probe 'e e)
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder as bs ss c)
  ;;; Exercise 3.30: Constructs a ripple-carry adder.
  (define (iter a b s c-out)
    (if (not (null? a))
      (let ((c-in (make-wire)))
        ;(println "fulladder" (length a) "to go")
        (full-adder (car a)
                    (car b)
                    c-in
                    (car s)
                    c-out)
        (iter (cdr a)
              (cdr b)
              (cdr s)
              c-in))))
  ;(println "making a" (length as) "-bit fulladder")
  (iter as bs ss c)
  'ok)


;;; The simulation

(define (after-delay delay action)
  ;;; Adds a new item after delay from the current time.
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  ;;; The driver procedure for the simulation.
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        ;(println the-agenda) (newline)
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(define (probe name wire)
  ;;; Adds an action that prints a name and the current
  ;;; value of a wire
  (add-action! wire
               (lambda ()
                 (println 
                   (current-time the-agenda) 
                   name 
                   "\t  New-value =" 
                   (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (simulate-half-adder)
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))
  ;(probe 'input-1 input-1)
  ;(probe 'input-2 input-2)
  (probe 'sum sum)
  (probe 'carry carry)

  (half-adder input-1 input-2 sum carry)
  (set-signal! input-1 1)
  (propagate)
  (set-signal! input-2 1)
  (propagate))


(define (simulate-ripple-adder)
  ;;; Simulates the addittion of two numbers using a ripple adder.
  (define (make-bus bit x)
    ;; Creates a list of wires, bits wide, with an intial value x.
    (if (= bit -1)
        '()
        (let ((a (make-wire))
              (n (expt 2 bit)))
          (set-signal! a (floor (/ x n)))
          (cons a 
                (make-bus (- bit 1) (remainder x n))))))
  (define (probe-bus name r)
    ;; Probes each wire in bus r
    (if (not (null? r))
        (begin  
          (probe (symbol name (length (cdr r))) (car r))
          (probe-bus name (cdr r)))))
  (let ((a (make-bus 3 3))
        (b (make-bus 3 5))
        (s (make-bus 3 0))
        (c (make-wire)))
    (probe-bus 's s) 
    (probe 'c c)
    (ripple-carry-adder a b s c)
    (propagate)))

;(simulate-half-adder)
(simulate-ripple-adder)
