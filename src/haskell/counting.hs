-- From http://stackoverflow.com/questions/9920129/how-to-count-the-number-of-times-a-fuction-was-called-the-fp-way
import Control.Monad.State

count func x = do
    num <- get
    put (num + 1)
	--modify ( + 1)
    return $ func x


counted func = count func

--square x = counted $ \x -> x * x
square x = x*x

runSomeFuncs = do
    count square 1
    count square 2
    count square 3

main = do
    let (res, state) = runState runSomeFuncs 0
    putStrLn ("result: " ++ (show res))
    putStrLn ("# of calls: " ++ show state)
