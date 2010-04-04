{-
	Project Euler Challenge 005:

	What is the smallest number divisible by each of the numbers 1 to 20?
-}

import List
import Maybe

prime_factors n
	| n == 1 = []
	| otherwise = smallest_factor : (prime_factors (n `div` smallest_factor))
	where smallest_factor = fromJust $ find (\x -> n `mod` x == 0) [2..n]

all_prime_factors upper =
	map prime_factors [2..upper]
	
unique_factors upper =
    -- The general idea here is to take the prime factors of all the numbers
    -- between 2 and the upper bound and then to merge the prime factor lists
    -- in such a way that individual prime factors do not appear more often
    -- than necessary.
	foldl (\x y -> x ++ (y \\ x)) [] $ all_prime_factors upper

challenge_005_naive upper =
	product $ unique_factors upper
	
main = do
	putStrLn ("Naive solution: " ++ (show (sort $ unique_factors 20)))
	putStrLn ("Naive solution: " ++ (show (challenge_005_naive 20)))
