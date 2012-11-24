; SICP Exercise 3.22
;   Dawie de Klerk
;   2012-11-24

(load "../utils.scm")

(define (make-queue)
  ;;; Represetntation of a queue using message passing style
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      ;; Predicate for testing if the queue is empty
      (null? front-ptr))

    (define (front-queue)
      ;; Returns the front of the queue
      (if (empty-queue?)
          (error "FRONT-QUEUE called with an emtpy queue")
          (car front-ptr)))

    (define (insert-queue! item)
      ;; Inserts an item at the back of the queue
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      ;; Deletes the item at the front of the queue
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))

    (define (print-queue)
      ;; Prints the contents of the queue
      (display front-ptr)
      (newline))

    (define (dispatch m)
      ;; Message dispatch
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "MAKE-QUEUE: Undefined operation" m))))

    dispatch))

;;;; Wrapper functions for queues
(define (empty-queue? queue)
  ((queue 'empty-queue?)))
(define (front-queue queue)
  ((queue 'front-queue)))
(define (delete-queue! queue)
  ((queue 'delete-queue!))
  queue)
(define (insert-queue! queue item) 
  ((queue 'insert-queue!) item) 
  queue)
(define (print-queue queue)
  ((queue 'print-queue))
  queue)

(define (do-example)
  (let ((q1 (make-queue)))
    (print-eval (insert-queue! q1 'a))
    (print-queue q1)
    (print-eval (insert-queue! q1 'b))
    (print-eval (front-queue q1))
    (print-eval (empty-queue? q1))
    (print-queue q1)
    (print-eval (delete-queue! q1))
    (print-queue q1)
    (print-eval (delete-queue! q1))
    (print-queue q1)
    (print-eval (empty-queue? q1))))
