-- SICP Example 1.2.6

find_divisor n test
	| test^2 > n = n
	| n`mod`test == 0 = test
	| otherwise = find_divisor n (test + 1)

smallest_divisor n = find_divisor n 2

isPrime n = n == smallest_divisor n

expmod base exp m 
	| exp == 0 = 1
	| even exp = (expmod base (exp`div`2) m)^2 `mod` m
	| otherwise = base * expmod $ base (exp-1) m



main = putStrLn $ show $ zip [1..] [isPrime n | n <- [1..10]]
