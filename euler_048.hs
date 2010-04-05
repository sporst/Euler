{-
	Project Euler Challenge 048:

	Find the last ten digits of 1^1 + 2^2 + ... + 1000^1000.
-}

challenge_048_naive =
	(sum [k^k | k <- [1..1000]]) `mod` (10 ^ 10)
	
main = do
	putStrLn ("Naive solution: " ++ (show challenge_048_naive))
