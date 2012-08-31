; SICP Exercise 2.35
;   Dawie de Klerk
;   2012-08-31

(load "../utils.scm")
(load "2.33.scm")

(define (count-leaves t)
  (accumulate 
    (lambda (x y) 
      (+ y x))
    0 
    (map (lambda (item)
           (if (not (pair? item)) 1
               (count-leaves item)))
         t)))


(define (enumerate-tree tree)
  ;; Enumerate tree from 2.2.3
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (enumerate-tree tree)
  (accumulate 
    (lambda (next acc)
      (if (not (pair? next)) 
          (cons next acc)
          (append (enumerate-tree next) acc)))
    '()
    tree))


(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

