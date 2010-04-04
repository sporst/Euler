{-
	Project Euler Challenge 004:

	Find the largest palindrome made from the product of two 3-digit numbers.
-}

import List

three_number_products = [x * y | x <- [100..999], y <- [100..999]]

challenge_004_naive = maximum [x | x <- three_number_products, show x == (reverse (show x))]

main = do
	putStrLn ("Naive solution: " ++ (show (challenge_004_naive)))
