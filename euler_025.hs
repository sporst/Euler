{-
	Project Euler Challenge 025:

	What is the first term in the Fibonacci sequence to contain 1000 digits?
-}

import List
import Maybe

fibonaccis =
	1 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

challenge_025_naive =
	1 + (fromJust $ findIndex (>=10^999) fibonaccis)
	
main = do
	putStrLn ("Naive solution: " ++ (show challenge_025_naive))
