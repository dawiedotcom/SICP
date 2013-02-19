; SICP Exersice 3.16
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define (do-examples)
  ; The easiest one:
  ;  |.|.| -> |.|.| -> |.|/|
  ;   |        |        |
  ;   a        b        c
  (print-eval (count-pairs '(a b c)))
  ; We can get 4 if one pair object could be
  ; double counted.
  ;  |.|.| - - \ 
  ;   |        |  
  ;  |.|.| -> |.|/|
  ;   |        |
  ;   a        b
  (define x '(a b))
  (print-eval (count-pairs (cons x (cdr x))))
  ; Count-pairs returns 7 if each of the first
  ; two pair objects points to the next element
  ; twice.
  ;  |.|.|
  ;   | |
  ;  |.|.|
  ;   | |
  ;  |.|/|
  ;   | 
  ;   a
  (define y '(a))
  (define z (cons y y))
  (print-eval (count-pairs (cons z z)))
  ; Count-pairs will never return for any structure
  ; that contains a loop.
  ; |.|.| -> |.|/|
  ;    \<- - /
  (set-cdr! (cdr x) x)
  (print-eval (count-pairs x)))
