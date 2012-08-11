-- SICP Exercise 1,12: Pascal's triangle

sum_pairs [] = []
sum_pairs [a,b] = [a+b]
sum_pairs (x:xs) = [x+head xs] ++ sum_pairs xs

triangle :: Int -> [Int]
triangle 1 = [1]
triangle 2 = [1, 1]
triangle n = [1] ++ (sum_pairs (triangle (n-1))) ++ [1]

--print_triangle n = [print (triangle i) | i <- [1..n]]
