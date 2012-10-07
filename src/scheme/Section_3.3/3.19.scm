; SICP Exersice 3.19
;   Dawie de Klerk
;   2012-10-07

(load "../utils.scm")
(define (cyclic? x)
  ;; Returns true if x contains any cycles that
  ;; would cause an infinite loop by cdr-ing through x,
  ;; but using only constant space.
  ;;  NOTE: This function will go into an infinite loop
  ;;    if the head is not contained in the cycle.
  (if (null? (cdr x))
      #f
      (let ((head x))
        (define (iter x)
          (cond ((null? (cdr x)) #f)
                ((eqv? x head) #t)
                ;((and (pair? (memq x visited))
                ;      (pair? (memq (cdr x) visited))) #t)
                (else
                  (begin (iter (cdr x))))))
        (iter (cdr x)))))

(define (cyclic? x)
  ;; The tortuise and hare algorithm. See
  ;;  [1]: http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare
  ;;  [2]: http://community.schemewiki.org/?sicp-ex-3.19
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (iter tortoise hare)
    (cond ((not (pair? tortoise)) #f)
          ((not (pair? hare)) #f)
          ((eq? tortoise hare) #t)
          ((eq? tortoise (safe-cdr hare)) #t)
          (else
           (iter (safe-cdr tortoise)
                 (safe-cdr (safe-cdr hare))))))
  (iter (safe-cdr x)
        (safe-cdr (safe-cdr x))))

(define (do-examples)
  ;; A normal list with three symbols
  (print-eval (cyclic? '(a b c)))
  ;; The first pair points to the second and third
  (define x '(a b))
  (print-eval (cyclic? (cons x (cdr x))))
  ;; Each pair points to the next twice
  (define y '(a))
  (define z (cons y y))
  ;; A cyclic 2 element list
  (print-eval (cyclic? (cons z z)))
  (set-cdr! (cdr x) x)
  (print-eval (cyclic? x))
  ;; A cyclic 1 element list
  (define w (cons 'a 'b))
  (set-cdr! w w)
  (print-eval (cyclic? w))
  ;; A cyclic 2 element list where the 
  ;; last element points to itself
  (define s (list 'a 'b))
  (set-cdr! (cdr s) (cdr s))
  (print-eval (cyclic? s)))
