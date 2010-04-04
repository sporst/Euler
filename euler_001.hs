{-
	Project Euler Challenge 001:

	Add all the natural numbers below one thousand that are multiples of 3 or 5.
-}

challenge_001_naive = 
	sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
	
sum_multiples k maximum =
	-- Recognize that summing all multiples of k equals summing all numbers from
	-- 1 to p = maximum / k and multiplying by k again. The sum of all integers
	-- between 1 and p can be calculated by that p * (p + 1) / 2 formula by
	-- Carl Gauss.
	let p = maximum `div` k
	in k * p * (p + 1) `div` 2
	
challenge_001_better =
    -- Calculate the individual sums and make sure not to count numbers
    -- divisible by 15 twice.
	sum_multiples 3 999 + sum_multiples 5 999 - sum_multiples 15 999
	
main = do
	putStrLn ("Naive Solution: " ++ (show challenge_001_naive))
	putStrLn ("Better Solution: " ++ (show challenge_001_better))
	