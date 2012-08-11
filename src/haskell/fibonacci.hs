-- SICP Section 1.2.2: Tree Recursion

-- The naive tree recursive way
fib_rec 0 = 0
fib_rec 1 = 1
fib_rec n = (fib_rec (n-1)) + (fib_rec (n-2))

-- A linear-recursve method
-- try [fib_rec n | n <- [1..100]] and 
-- 		[fib_iter n | n <- [1..100]] for 
-- 		a comparison in performance!
fib_iter n = fib_iter_ n 1 0

fib_iter_ 0 a b = b
fib_iter_ n a b = fib_iter_ (n-1) (a+b) a
