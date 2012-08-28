; SICP Exercise 2.25
;   Dawie de Klerk
;   2012-08-25

(define list1 '(1 3 (5 7) 9))
(display (car (cdr (car (cdr (cdr list1))))))

(define list2 '((7)))
(display (car (car list2)))

(define list3 '(1 (2 (3 (4 (5 (6 7)))))))
(display (car (cdr 
                (car (cdr 
                       (car (cdr 
                              (car (cdr 
                                     (car (cdr 
                                            (car (cdr list3)))))))))))))
