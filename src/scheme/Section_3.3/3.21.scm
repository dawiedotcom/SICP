; SICP Exercise 3.21
;   Dawie de Klerk
;   2012-11-24

(load "queue.scm")
(load "../utils.scm")

(define (print-queue queue)
  ;; Prints the content of a queue. The queue object is
  ;; a pair of pointers to the front and rear of a queue.
  ;; For it te be printed correctly only the list that 
  ;; the front is pointing to is printed.
  (display (front-ptr queue))
  (newline))

(define (do-example)
  (let ((q1 (make-queue)))
    (print-eval (insert-queue! q1 'a))
    (print-queue q1)
    (print-eval (insert-queue! q1 'b))
    (print-queue q1)
    (print-eval (delete-queue! q1))
    (print-queue q1)
    (print-eval (delete-queue! q1))
    (print-queue q1)))
