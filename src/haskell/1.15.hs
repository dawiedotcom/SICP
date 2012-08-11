-- SICP Execise 1.15


p :: (Floating a) => a -> a
p x = 3*x - 4*x^3

--sin' :: (Floating a) => a -> a
sin' x
	| abs x < 0.1 = x
	| otherwise = p (sin' (x/3))

count_sin (x,n) = (sin' x, n+1)
