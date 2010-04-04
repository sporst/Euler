{-
	Project Euler Challenge 010:

	Calculate the sum of all the primes below two million.
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
            
challenge_010_naive =
	sum $ primesToN 2000000
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_010_naive)))
