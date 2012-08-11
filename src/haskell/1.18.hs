-- SICP Exercise 1.17

double a = a + a
half a   = a `div` 2

mul' _ 0 n = n
mul' a b n
	| even b = mul' (double a) (half b) n
	| otherwise = mul' a (b-1) (n + a)

mul a b = mul' a b 0

main = do
	putStrLn $ show [mul 2 i | i <- [0..10]]
	putStrLn $ show [mul i 2 | i <- [0..10]]
	putStrLn $ show [mul 5 i | i <- [0..10]]
	putStrLn $ show [mul i 5 | i <- [0..10]]
