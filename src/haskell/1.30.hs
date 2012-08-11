-- SICP Exercise 1.30 

sigma term a next b = iter a 0
	where
		iter a result
			| a > b = result
			| otherwise = iter (next a) (result + term a)


main = putStrLn $ show $ sigma (\x->x) 0 (\x->x+1) 10
