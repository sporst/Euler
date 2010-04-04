{-
	Project Euler Challenge 006:

	What is the difference between the sum of the squares and the square of the
	sums?
	
	The naive solution simply calculates the sum of the squares and the square of
	the sums. The better solution recognizes that both values can be calculated
	using a direct formula. To calculate the square of the sums, the same formula
	is used that was already used in the solution to challenge 001. To calculate
	the sum of the squares the formula sum(k=1 to n)k^2 = (n*(n+1)*(2n+1))/6 was
	used.
-}

challenge_006_naive :: Integer -> Integer
challenge_006_naive upper =
	let 
		sum_of_squares = sum [x * x | x <- [1..upper]]
		square_of_sums = (sum [1..upper]) ^ 2
	in
		square_of_sums - sum_of_squares

challenge_006_better :: Integer -> Integer
challenge_006_better upper = 
	let
		square_of_sums = (upper * (upper + 1) `div` 2) ^ 2
		sum_of_squares = upper * (upper + 1) * (2 * upper + 1) `div` 6
	in
		square_of_sums - sum_of_squares 
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_006_naive 100)))
	putStrLn ("Better solution: " ++ (show (challenge_006_better 100)))
