; SICP Exersice 3.12
;   Dawie de Klerk
;   2012-10-06

(load "../utils.scm")

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (do-examples)
  (define z (make-cycle (list 'a 'b 'c)))
  ; z is now a circular list:
  ;       |<- - - - - - - - |
  ; z -> |.|.| - |.|.| - |.|.|
  ;       |       |       |
  ;       a       b       c
  ; (last-pair z) will get stuck in an 
  ; infinite loop, because the CDR of 
  ; none of z's elements are '(). In
  ; fact, even (display z) never returns.
  ;(println "z\n" z))
  )
