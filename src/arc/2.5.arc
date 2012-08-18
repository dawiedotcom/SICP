; SICP Exercise 2.5
;   Dawie de Klerk
;   2012-08-18

(def count-factor (n factor)
  ;; Counts the number of times that factor divides into
  ;; n with no remainder.
  (def iter (x c)
    (if (> (mod x factor) 0)
        c
        (iter (/ x factor) (inc c))))
  (iter n 0))

;;; New cons, car and cdr

(def conss (a b)
  (* (expt 2 a) (expt 3 b)))

(def carr (p)
  (count-factor p 2))
(def cdrr (p)
  (count-factor p 3))

;;; Some tests

(def test ()
  (do 
      (prn "car of (2 2) " (carr (conss 2 2)))
      (prn "cdr of (2 2) " (cdrr (conss 2 2)))
      (prn "car of (3 2) " (carr (conss 3 2)))
      (prn "cdr of (3 2) " (cdrr (conss 3 2)))
      (prn "car of (1 1) " (carr (conss 1 1)))
      (prn "cdr of (1 1) " (cdrr (conss 1 1)))
      (prn "car of (10 100) " (carr (conss 10 100)))
      (prn "cdr of (10 100) " (cdrr (conss 10 100)))))
