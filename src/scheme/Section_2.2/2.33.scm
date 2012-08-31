; SICP Exercise 2.33
;   Dawie de Klerk
;   2012-08-31

(load "../utils.scm")

(define nil '())

(define (accumulate op initial sequence)
  ;; Accumulate as in the text of the book
  ;;  NOTE that the arguments given to op are reversed from
  ;;  mit-scheme's fold, but the same order as fold rigth.
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) 
                (cons (p x) y))
              nil
              sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
