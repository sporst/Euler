{-
	Project Euler Challenge 007:

	Find the 10001st prime.
	
	The solution is a pretty slow. I don't like prime number challenges so I went
	for the naive O(n^2) solution without setting up a proper sieve or doing even
	fancier stuff. Instead I just brute-force the solution by filtering the
	numbers based on the number of their prime factors.
	
	Possible improvements even in the brute force version:
		- Skip all even numbers
		- Skip further calculation of prime factors if more than 2 are found
-}

import List
import Maybe

prime_factors n
	| n == 1 = []
	| otherwise = smallest_factor : (prime_factors (n `div` smallest_factor))
	where smallest_factor = fromJust $ find (\x -> n `mod` x == 0) [2..n]

is_prime n =
	length (prime_factors n) == 1
	
challenge_007_naive index =
	(filter is_prime [2..]) !! (index - 1)
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_007_naive 10001)))
