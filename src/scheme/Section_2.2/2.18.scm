; SICP Exercise 2.17
;   Dawie de Klerk
;   2012-08-20

(load "../utils.scm")

(define (reverse1 lst)
  ;; Reverse the order of lst
  (define (iter acc l)
    (if (zero? (length l))
        acc
        (iter (append acc (list (car l))) (cdr l))))
  (iter '() lst))

(define (last-reverse2 lst)
  ;; Reverse the order of list using fold
  (fold (lambda (elem acc) (append acc (list elem))) 
        '() 
        lst))

;;; TEST
(define (test-reverse)
  (println "last-pair (23 72 149 34):" (reverse1 (list 23 72 149 34)))
  (println "last-pair (1 2 3 4 5 6 7):" (reverse1 (list 1 2 3 4 5 6 7)))
  (println "last-pair (7):" (reverse1 (list 7))))
(define (test-reverse2)
  (println "last-pair (23 72 149 34):" (reverse2 (list 23 72 149 34)))
  (println "last-pair (1 2 3 4 5 6 7):" (reverse2 (list 1 2 3 4 5 6 7)))
  (println "last-pair (7):" (reverse2 (list 7))))
