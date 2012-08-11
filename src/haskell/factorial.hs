-- SICP Section 1.2.1: Factorial 

-- A recursive definition for factiorial
factorial_rec 1 = 1
factorial_rec n = n * factorial_rec (n-1)


-- An itarative definition for factorial
factorial_iter n = factorial_iter_ n 1 1

factorial_iter_ n count acc = 
	if n == count
	then acc
	else factorial_iter_ n next (acc*next)
		where next = count + 1
