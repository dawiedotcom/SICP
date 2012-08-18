; SICP Exercise 2.4
;   Dawie de Klerk
;   2012-08-18

;; We are given the following definitions of cons and car

(def conss (x y)
  (fn (m) (m x y)))

(def carr (z)
  (z (fn (p q) p)))

;; cdr calls the function that represents the pair and returns
;; the secord argument

(def cdrr (z)
  (z (fn (p q) q)))

;; Some test

(def test ()
  (do (prn "car of ('x 'y) " (carr (conss 'x 'y)))
      (prn "cdr of ('x 'y) " (cdrr (conss 'x 'y)))
      (prn "car of (10 100) " (carr (conss 10 100)))
      (prn "cdr of (10 100) " (cdrr (conss 10 100)))))
