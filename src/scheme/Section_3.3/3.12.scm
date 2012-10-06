; SICP Exersice 3.12
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (do-examples)
  (define x (list 'a 'b))
  (println x)
  ; x -> |.|.| - |.|/|
  ;       |       |
  ;       a       b
  (define y (list 'c 'd))
  (println y)
  ; y -> |.|.| - |.|/|
  ;       |       |
  ;       c       d
  (define z (append x y))
  (println "(define z (append x y))\n" z)
  (print-eval (cdr x))
  ; (cdr x) is x with the first pair removed.
  ; This works be cause APPEND leaves its arguments
  ; unchanged.
  ; (cdr x) -> |.|/|
  ;             |
  ;             b
  ; which is a list with a single element. 
  ; In the repl we get
  ; => (b)
  ; as expected.
  (define w (append! x y))
  ; This operation changes x! w and x are now the
  ; same object, the list (a b c d):
  ; w,x -> |.|.| - |.|.| - |.|.| - |.|/|
  ;         |       |       |       |
  ;         a       b       c       d
  (println "(define w (append! x y))\n" w)
  (print-eval (cdr x))
  ; Because APPEND! mutated x, a call to the same 
  ; proceduce with the same arguments, produces 
  ; different results:
  ; (car x) -> |.|.| - |.|.| - |.|/|
  ;             |       |       |
  ;             b       c       d
  (println "x\n" x))
