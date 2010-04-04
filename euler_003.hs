{-
	Project Euler Challenge 003:

	Find the largest prime factor of a composite number.
-}

import List
import Maybe

prime_factors n
	| n == 1 = []
	| otherwise = smallest_factor : (prime_factors (n `div` smallest_factor))
	where smallest_factor = fromJust $ find (\x -> n `mod` x == 0) [2..n]

challenge_003_naive = maximum . prime_factors

main = do
	putStrLn ("Naive solution: " ++ (show (prime_factors 600851475143)))
	putStrLn ("Naive solution: " ++ (show (challenge_003_naive 600851475143)))
