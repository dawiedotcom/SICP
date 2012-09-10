; SICP Section 2.3.3
;   Dawie de Klerk
;   2012-09-08

(load "../utils.scm")

;;; These are the set operations defined in the text. Sets are
;;; represented as unordered lists without no duplicates.

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59 -- Union-set for unordered list representation of sets.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (union-set (cdr set1) 
                    (cons (car set1) set2)))))

;;; Exercise 2.60 -- Represent sets as lists with duplicates.


(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;;; Representing sets as ordered lists.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else
          (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1)
          (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61 -- Implement adjoin-set for the ordered list rep

(define (adjoin-set x set)
  ;; A first attempt, but its obviously wrong because of all the 
  ;; appending.
  (define (iter before after)
    (cond ((null? after) before)
          ((< x (car after)) 
           (append before (cons x after)))
          (else
           (iter (append before (list (car after))) 
                 (cdr after)))))
  (iter '() set))

(define (adjoin-set x set)
  ;; Solution from 
  ;;  [1]: http://www.billthelizard.com/2012/07/sicp-261-262-sets-as-ordered-lists.html 
  ;; This is much better, since appending as in the above solution iterates
  ;; over the entire list. This will only iterate to the point at which x
  ;; needs to be inserted into the list or where the equal element already is.
  (let ((head (car set))
        (tail (cdr set)))
    (cond ((null? set) (list x))
          ((= x head) set)
          ((< x head)
           (cons x set))
          ((> x head)
           (cons head
                 (adjoin-set x tail))))))

;;; Exercise 2.62 -- union-set in O(n)

(define (union-set set1 set2)
  (println set1 set2 (null? set1) (null? set2))
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((> x1 x2)
                   (cons x2 (union-set set1 (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2)))))))))
