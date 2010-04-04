{-
	Project Euler Challenge 016:

	What is the sum of the digits of the number 2^1000?
-}

import Char

challenge_016_naive =
	sum $ map digitToInt (show $ 2 ^ 1000)
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_016_naive)))
