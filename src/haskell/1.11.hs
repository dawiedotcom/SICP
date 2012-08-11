-- SICP Exercise 1.11

-- f(n) recursive
f_rec n
	| n < 3 = n
	| otherwise = f_rec (n-1) + 2 * f_rec (n-2) + 3 * f_rec (n-3)

-- f(n) iterative
f_iter n = f_iter_ n 2 1 0

f_iter_ 0 _ _ c = c
f_iter_ n a b c = f_iter_ (n-1) (a + 2*b + 3*c) a b
