-- SICP Execise 1.16



fast_exp' _ 0 a = a
fast_exp' b n a
	| n `mod` 2 == 0 = fast_exp' (b*b) (n`div`2) (a)
	| otherwise = fast_exp' b (n-1) (a*b)

fast_exp b n = fast_exp' b n 1

--main = putStrLn $ show $ [fast_exp' 2 i 1 | i <- [0,2,4,8]]
main = putStrLn $ show $ [fast_exp 2 i | i <- [0..8]]
	
