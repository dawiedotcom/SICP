; SICP Section 2.3.3
;   Dawie de Klerk
;   2012-09-09

(load "../utils.scm")

;;; A binary tree represented as a list (entry left-branch right-branch)
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;; A set represented in terms of a binary tree
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define (do-examples)
  ;; Both tree->list-1 and tree->list-2 produce an ordered list
  ;; of the tree elements. They compute essentially the same
  ;; process, with the difference that the first uses append
  ;; and cons to join lists where the second only uses cons. If
  ;; append and cons can both be counted as a 'step', both
  ;; procedures are O(n). 
  (let ((tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
        (tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
        (tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
    (print-eval (tree->list-1 tree1))
    (print-eval (tree->list-1 tree2))
    (print-eval (tree->list-1 tree3))
    (print-eval (tree->list-2 tree1))
    (print-eval (tree->list-2 tree2))
    (print-eval (tree->list-2 tree3))))

;;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  ;; Partial-tree constructs a balanced tree by dividing the list into
  ;; two parts by choosing a pivot element from the middle of the 
  ;; list. The left and right branches are constructed by recursive 
  ;; calls on the first and second half of the list, respectively.
  ;; The pivot element is used as the entry of the root node.
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; (tree->list '(1 3 5 7 9 11)) results in the following tree
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;      5
;;    /   \
;;   1     9
;;    \   | \
;;     3  7 11
;; The order of growth is O(log n) - since the size of the 
;; list passed to partial-tree halves at each step.


;;; Exercise 2.65 -- The conversion between ordered lists and trees
;;;     suggest to use the following strategy
;;;       (list->tree (<list-op> (tree->list s1) (tree->list s2)))
;;;     Since the order of the combined operation will be the order
;;;     of the most expensive operation, which is O(n) in both cases,
;;;     both union-set and intersection-set will be O(n) too.

(define (set-operation proc set1 set2)
  (list->tree (proc (tree->list-2 set1)
                    (tree->list-2 set2))))

(define (union-set set1 set2)
  (define (list-union l1 l2)
    ; union-set on ordered lists form the previous section
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((> (car l1) (car l2))
           (cons (car l2)
                 (list-union l1 (cdr l2))))
          ((< (car l1) (car l2))
           (cons (car l1)
                 (list-union (cdr l1) l2)))
          (else
           (cons (car l1)
                 (list-union (cdr l1) (cdr l2))))))

  (set-operation list-union set1 set2))
    
(define (intersection-set set1 set2)
  (define (list-intersection s1 s2)
    ; intersection-set on ordered lists form the previous section
    (if (or (null? s1)
            (null? s2))
        '()
        (let ((x1 (car s1))
              (x2 (car s2)))
          (cond ((= x1 x2)
                 (cons x1
                       (list-intersection (cdr s1)
                                         (cdr s2))))
                ((< x1 x2)
                 (list-intersection (cdr s1) s2))
                ((< x2 x1)
                 (list-intersection s1 (cdr s2))))))) 

  (set-operation list-intersection set1 set2))

;;; Run some examples
(define (do-examples)
  (print-eval (tree->list-1 (intersection-set 
                (list->tree '(6 7 8)) 
                (list->tree '(1 4)))))
  (print-eval (tree->list-1 (intersection-set 
                (list->tree '(6 7 8 10)) 
                (list->tree '(4 5 7 10)))))
  (print-eval (tree->list-1 (union-set 
                (list->tree '( 6 7 8)) 
                (list->tree '(1 2 3 4 5)))))
  (print-eval (tree->list-1 (union-set 
                (list->tree '(6 7 8)) 
                (list->tree '(4 5 6))))))
