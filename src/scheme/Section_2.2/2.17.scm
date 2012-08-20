; SICP Exercise 2.17
;   Dawie de Klerk
;   2012-08-20

(load "../utils.scm")

(define (last-pair lst)
  ;; Returns a list with the last element in lst.
  (if (= (length lst) 1)
      lst
      (last-pair (cdr lst))))

(define (last-pair2 lst)
  ;; Last pair in term of fold
  (fold (lambda (a b) (list a)) '() lst))

;;; TEST
(define (test-last-pair)
  (println "last-pair (23 72 149 34):" (last-pair (list 23 72 149 34)))
  (println "last-pair (1 2 3 4 5 6 7):" (last-pair (list 1 2 3 4 5 6 7)))
  (println "last-pair (7):" (last-pair (list 7))))
(define (test-last-pair2)
  (println "last-pair (23 72 149 34):" (last-pair2 (list 23 72 149 34)))
  (println "last-pair (1 2 3 4 5 6 7):" (last-pair2 (list 1 2 3 4 5 6 7)))
  (println "last-pair (7):" (last-pair2 (list 7))))
