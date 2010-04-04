{-
	Project Euler Challenge 014:

	Find the longest sequence using a starting number under one million.
-}

import Data.List

next n
	| even n = n `div` 2
	| otherwise = 3 * n + 1
	
calc_sequence start =
	(takeWhile (/=1) $ iterate next start) ++ [1]
	
all_sequences upper =
	[(s, length $ calc_sequence s) | s <- [2..upper]]

max_sequence (a, b) (c, d)
	| b > d = (a, b)
	| otherwise = (c, d)
		
maximum_sequence upper =
	foldl1' max_sequence (all_sequences upper)
	
challenge_014_naive =
	maximum_sequence 1000000

main = do
	putStrLn ("Naive solution: " ++ (show (challenge_014_naive)))
