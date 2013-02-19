; SICP Exercise 3.23
;   Dawie de Klerk
;   2012-11-24

(load "queue.scm")
(load "../utils.scm")


;;; Instead of using a list as with a queue, we need to use a
;;; doubly linked list in order to have all the operations
;;; O(1).

;;; Represent an elem in the doubly linked list as 
;;; ((prev . next) item)
(define (make-elem item next prev)
  (cons (cons prev next) item))

(define (set-next! elem new-next)
  (set-cdr! (car elem) new-next))

(define (set-prev! elem new-prev)
  (set-car! (car elem) new-prev))

(define (item elem) (cdr elem))
(define (next elem) (cdar elem))
(define (prev elem) (caar elem))

;;; A deque has pointer to the beginning and end of a doubly 
;;; linked list. We can reuse the front and rear procedures 
;;; from the queue example.

(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (empty-queue? deque))

(define (front-deque deque)
  ;; Returns the item at the front of the queue
  (if (empty-deque? deque)
      (error "FRONT called with empty deque" deque)
      (item (front-queue deque))))

(define (rear-deque deque)
  ;; Returns the element at te rear of the deque.
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item (rear-ptr deque))))

(define (front-insert-deque! deque item)
  ;; Adds item to the front of the deque
  (let ((new-elem (make-elem item (front-ptr deque) '())))
    (cond ((empty-deque? deque)
           (rear-insert-deque! deque item))
          (else
           (set-prev! (front-ptr deque) new-elem)
           (set-front-ptr! deque new-elem)))))

(define (rear-insert-deque! deque item)
  ;; Adds item to the rear of the deque
  (let ((new-elem (make-elem item '() (rear-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-elem)
           (set-rear-ptr! deque new-elem)
           deque)
          (else
           (set-next! (rear-ptr deque) new-elem)
           (set-rear-ptr! deque new-elem)))))

(define (front-delete-deque! deque)
  ;; Removes the item at the front of the deque
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((new-front (next (front-ptr deque))))
           (set-next! (front-ptr deque) '())
           (set-front-ptr! deque new-front)
           (if (null? new-front)
               ; Special case for when the last item was removed
               (set-rear-ptr! deque '())
               (set-prev! (front-ptr deque) '()))))))


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((new-rear (prev (rear-ptr deque))))
           (set-prev! (rear-ptr deque) '())
           (set-rear-ptr! deque new-rear)
           (if (null? new-rear)
               ; Special case for when the last item was removed
               (set-front-ptr! deque '())
               (set-next! (rear-ptr deque) '()))))))

(define (print-deque deque)
  ;; Prints a deque, because it contains cycles.
  (define (iter elem)
    (display (item elem))
    (if (not (null? (next elem)))
        (begin (display " ")
               (iter (next elem)))))
  (display "(")
  (if (not (empty-deque? deque))
      (iter (front-ptr deque)))
  (display ")")
  (newline))
        
(define (do-examples)
  ;; Do some simple tests
  (let ((d1 (make-deque)))
    (front-insert-deque! d1 'b)
    (print-deque d1)
    (front-insert-deque! d1 'a)
    (print-deque d1)
    (rear-insert-deque! d1 'c)
    (print-deque d1)
    (front-delete-deque! d1)
    (print-deque d1)
    (rear-delete-deque! d1)
    (print-deque d1)
    (rear-delete-deque! d1)
    (print-deque d1)
    (front-insert-deque! d1 'x)
    (print-deque d1)
    (front-delete-deque! d1)
    (print-deque d1)))
