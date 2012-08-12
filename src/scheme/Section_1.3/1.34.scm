; SICP Exercise 1.34
;   Dawie de Klerk
;   2012-08-12


(define (f g)
  (g 2))

; Evaluating f on itself, by substituting f into 
; its definition we get
;   (f f)
;   (f 2)
;   (2 2)
; which explains the error in the scheme REPL:
;   'The object 2 is not applicable.'
; We are trying to call 2 as a function, with 2
; as the argument.
