-- SICP Exercise 1.17

double a = a + a
half a   = a `div` 2
--even n   = n `mod` 2 == 0


mul _ 0 = 0
mul a 1 = a
mul a b
	| even b = double $ mul (a) (half b)
	| otherwise = a + mul a (b-1)

main = do
	putStrLn $ show [mul 2 i | i <- [0..10]]
	putStrLn $ show [mul i 3 | i <- [0..10]]
