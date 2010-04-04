{-
	Project Euler Challenge 020:

	Find the sum of digits in 100!
-}

import Char

challenge_020_naive =
	sum $ map digitToInt (show $ product [1 .. 100])
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_020_naive)))
