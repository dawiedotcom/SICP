-- SICP Exercise 1.3

largest a b c = max (max a b) (max b c)
median a b c = max (min a b) (min b c)

sumOfSquares a b c = (largest a b c)^2 + (median a b c)^2
