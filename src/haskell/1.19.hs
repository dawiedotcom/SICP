-- SICP Exercise 1.19

fib_iter _ b _ _ 0 = b
fib_iter a b p q count
	| even count = fib_iter a b p' q' (count`div`2)
	| otherwise  = fib_iter a' b' p q (count-1)
	where 
		a' = b*q + a*q + a*p
		b' = b*p + a*q
		p' = q*q + p*p
		q' = q*q + 2*p*q

fib n = fib_iter 1 0 0 1 n

main = putStrLn $ show [fib n | n <- [0..10]]
