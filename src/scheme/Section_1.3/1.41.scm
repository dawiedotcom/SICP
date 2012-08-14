; SICP Exercise 1.41
;   Dawie de Klerk
;   2012-08-13

(define (double g)
  ;; Takes a procedure g and returns a procedure 
  ;; that applies g twice.
  (lambda (x)
    (g (g x))))

; (((double (double double)) inc) 5)
; (((double (lambda (g) (double (double g)))) inc) 5)
; (((lambda (gg) 
;       (double (double (double (double gg))))) inc) 5)
; ( ... inc 16 times ... 5)
