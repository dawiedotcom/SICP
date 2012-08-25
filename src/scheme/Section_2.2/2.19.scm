; SICP Exercise 2.19
;   Dawie de Klerk
;   2012-08-25

(load "../utils.scm")

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

;; Selectors for a list of coins in terms of lists.
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (no-more? coins)
  (zero? (length coins)))

;; Some test
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (test-cc)
  (println "(cc 100 us-coins)" (cc 100 us-coins))
  ;; The order of the list should not make a difference, because the 
  ;; actual coin values are stored in the list.
  (println "(cc 100 (reverse us-coins))" (cc 100 (reverse us-coins))))
