; SICP Exersice 3.14
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (mystery x)
  ; Reverses a list x into a new list.
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define (do-examples)
  (define v '(a b c d))
  ; v -> |..|-|..|-|..|-|./|
  ;       a    b    c    d
  (println "v\n" v)
  (define w (mystery v))
  (println "v\n" v)
  ; v -> |./|
  ;       a 
  (println "w\n" w))
  ; w -> |..|-|..|-|..|-|./|
  ;       d    c    b    a
