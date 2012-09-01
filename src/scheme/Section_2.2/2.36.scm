; SICP Exercise 2.36
;   Dawie de Klerk
;   2012-09-01

(load "../utils.scm")
;(load "2.33.scm") we need the built in map

(define (accumulate op initial sequence)
  ;; Accumulate as in the text of the book
  ;;  NOTE that the arguments given to op are reversed from
  ;;  mit-scheme's fold, but the same order as fold rigth.
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
          
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init 
                        (map car seqs)) ; Get list of all the first elements
            (accumulate-n op init 
                          (map cdr seqs))))) ; Get a list of all the tails

(define (do-example)
  (let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
    (print-eval s)
    (print-eval (accumulate-n + 0 s))))
