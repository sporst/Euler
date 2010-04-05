{-
	Project Euler Challenge 015:

	Starting in the top left corner in a 20 by 20 grid, how many routes are there
	to the bottom right corner?
	
	The naive solution calculates the complete Pascal triangle for each run. The
	better solution makes use of combinatorics to calculate the result directly.
-}

split_every n list
	-- Takes a list and splits it into evenly sized chunks.
	| length list < n = []
	| otherwise = (take n list) : (split_every n (drop 1 list))
	
next_line_upper line =
	[1] ++ map sum (split_every 2 line) ++ [1]
	
next_line_lower line =
	map sum (split_every 2 line)

calculate_triangle grid_size =
	let upper_half = (iterate next_line_upper [1, 1]) !! (grid_size - 1)
	in head $ dropWhile (\x -> length x /= 1) (iterate next_line_lower upper_half)

challenge_015_naive =
	calculate_triangle 20
	
calculate_combinations grid_size =
	product [1..2 * grid_size] `div` (product [1..grid_size] * product [1..grid_size])
	
challenge_015_better =
	calculate_combinations 20
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_015_naive)))
	putStrLn ("Better solution: " ++ (show (challenge_015_naive)))
