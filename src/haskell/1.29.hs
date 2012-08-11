-- SICP Exercise 1.29

--sigma f a b step = sum [f i | i <- [a,a+step..b]]

sigma term a next b
	| a > b = 0
	| otherwise = (term a) + sigma term (next a) next b

simpson f a b n = (h/3)* sigma (\x -> f (x) + 4*f (x+h) + f (x+2*h)) a (\x -> x+2*h) (b-h)
	where
		h = (b-a)/n

--main = putStrLn $ show $ sigma (\x -> x) 1 (\x -> x+1) 10
main = putStrLn $ show $ simpson (\x->x**3) 0 1 100
