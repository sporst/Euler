{-
	Project Euler Challenge 002:

	Find the sum of all the even-valued terms in the Fibonacci sequence which do
	not exceed four million.
-}

fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis)
		
challenge_002_naive =
	sum [x | x <- takeWhile (<4000000) fibonaccis, even x]

even_fib n
    -- It is possible to calulate the n-th even Fibonacci number directly
    -- from the two even Fibonacci numbers before it. Using this way it is not
    -- necessary to calculate the whole Fibonacci sequence.
	| n == 0 = 0
	| n == 1 = 2
	| otherwise = 4 * even_fib (n - 1) + even_fib (n - 2)

challenge_002_better =
	sum [x | x <- takeWhile (<4000000) (map even_fib [1..])]
			
main = do
	putStrLn ("Naive solution: " ++ (show challenge_002_naive))
	putStrLn ("Better solution: " ++ (show challenge_002_better))
