; SICP Section 3.3.2
;   Dawie de Klerk
;   2012-11-24

;;; A queue is represented as a pair of pointers. The car pointing to
;;; the front and the cdr to the rear of a list.

;;; Some helper procedures to manipulate the front and rear pointers
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))


;;; Queue operations
(define (empty-queue? queue) 
  ;; A queue is empty if the front pointer is the empty list.
  (null? (front-ptr queue)))

(define (make-queue)
  ;; Creates an empty queue
  (cons '() '()))

(define (front-queue queue)
  ;; Returns the first item in the queue.
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  ;; Adds a new item to the back of a queue.
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  ;; Removes the first item of the queue.
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
