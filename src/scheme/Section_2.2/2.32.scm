; SICP Exercise 2.31
;   Dawie de Klerk
;   2012-08-25

(define (subsets s)
  (if (null? s)
      (list '())    ; Subsets of the empty set is the set containing the empty set
      (let ((rest (subsets (cdr s))))   ; Make rest all the subsets of the tail of s
                                        ; The subsets of s, 
        (append rest                    ; is the subsets of its tail plus
                (map (lambda (sub)      ; Its head added
                       (cons (car s) sub)) ;
                     rest)))))          ; to each subset in its tail
