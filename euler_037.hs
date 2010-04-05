{-
	Project Euler Challenge 037:

	Find the sum of all eleven primes that are both truncatable from left to
	right and right to left.
	
	The naive solution simply brute-forces the result. It might be smarter to
	construct the prime numbers inductively. All truncatable prime numbers
	can only start with [2, 3, 5, 7] and must afterwards only contain
	[1, 3, 7, 9].
-}

import List
import Maybe
import Array

primesToN n = 2: [i | i <- [3,5..n], ar ! i]
	-- Sieve shamelessly stolen from
	-- http://www.haskell.org/haskellwiki/Prime_numbers
	where
		ar = f 5 $ accumArray (\ a b -> False) True (3,n) [(i,()) | i <- [9,15..n]]
		f p a
			| q > n = a
			| True  = if null x then a' else f (head x) a'
			where
				q = p*p
				a'= a // [(i,False) | i <- [q,q+2*p..n]]
				x = [i | i <- [p+2,p+4..n], a' ! i]

side_truncate f n =
	let
		digits = floor $ logBase 10 $ fromIntegral n
	in
		map (\x -> f n (10^x)) [1 .. digits]

left_truncate :: Integer -> [Integer]
left_truncate n = side_truncate mod n

right_truncate :: Integer -> [Integer]
right_truncate n = side_truncate div n

is_prime_2 n already_split
	-- To test whether a number is prime we check if it has more than one
	-- prime factor.
	| n == 1 = already_split
	| already_split = n == 1
	| otherwise = is_prime_2 (n `div` smallest_factor) True
	where smallest_factor = fromJust $ find (\x -> n `mod` x == 0) [2..n]

is_prime n =
	is_prime_2 n False
	
is_truncatable_prime n =
	all is_prime (left_truncate n ++ right_truncate n)

find_truncatable_primes upper =
		filter is_truncatable_prime $ (drop 4 (primesToN upper))

challenge_037_naive =
	sum $ take 11 $ find_truncatable_primes 1000000
	
main = do
	putStrLn ("Naive solution: " ++ (show challenge_037_naive))
