{-
	Project Euler Challenge 024:

	What is the millionth lexicographic permutation of the digits
	0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-}

import Data.List

challenge_024_naive =
	sort (permutations "0123456789") !! 999999

main = do
	putStrLn ("Naive solution: " ++ (challenge_024_naive))
