; SIPC Section 2.3.4
;   Dawie de Klerk
;   2012-09-15

(load "../utils.scm")

;;; Leafs as 
;;;   (list 'leaf symbol weight)
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

;;; Trees as
;;;   (list left-branch right-branch symbols weight)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;; More set operations
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
;; The tree is
;;    {A B C D} 8
;;    /       \
;;  {A} 2     {B C D} 4
;;            /     \
;;        {B} 2     {C D} 2
;;                 /    \
;;              {C} 1   {D} 1
;; Giving the following encoding:
;;   A: 0
;;   B: 10
;;   C: 110
;;   D: 111
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; This message reads:   A|  D  |A| B | B |  C  |A

(define (do-2-67)
  (print-eval (decode sample-message sample-tree)))
;; Returns (a d a b b c a) 

;;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0
               (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1
               (encode-symbol symbol (right-branch tree))))
        (else
         (error "symbol not in tree"))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (do-2-68)
  (print-eval (encode '(A D A B B C A) sample-tree))
  (print-eval (equal? sample-message (encode '(A D A B B C A) sample-tree))))

;;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
        (adjoin-set (make-code-tree (car leaves)
                                    (cadr leaves))
                    (cddr leaves)))))

(define (do-2-69)
  (print-eval (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
  (print-eval (equal? 
                sample-tree 
                (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))))

;;; Exercise 2.70

(define song-tree
  (generate-huffman-tree '((A 2)
                           (NA 16)
                           (BOOM 1)
                           (SHA 3)
                           (GET 2)
                           (YIP 9)
                           (JOB 2)
                           (WAH 1))))

(define song-lyrics
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(define (do-2-70)
  (print-eval (encode song-lyrics song-tree))
  (let ((song-code (encode song-lyrics song-tree)))
    (println "Message length:" (length song-code))
    (println "Length needed for a fixed-legth code:" 
             (* 3 (length song-lyrics)))))
