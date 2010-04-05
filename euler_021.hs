{-
	Project Euler Challenge 021:

	Evaluate the sum of all amicable pairs under 10000.
-}

import List
import Maybe

flatten f =
	foldl (++) [] f

divisors n =
	flatten [[k, n `div` k] | k <- [1.. floor $ sqrt $ fromIntegral n], n `mod` k == 0]

proper_divisors n =
	-- Make sure that each divisor only appears once and that n itself is not a
	-- divisor of n.
	nub $ divisors n \\ [n]
	
proper_divisor_list upper =
	-- Get a list of all divisors between 0 and the upper limit.
	map proper_divisors [0..upper]
	
proper_divisor_sums upper =
	-- Get a list of the sums of all divisors between 0 and the upper limit.
	map sum $ proper_divisor_list upper
	
get_partner sums index
	-- Find the amicable partner of a given number. If no amicable partner exists
	-- an empty list is returned.
	| 	sums !! index < length sums &&     -- Make sure we do not go out of bounds.
		sums !! index < index &&           -- Make sure to skip duplicates and (x, x)
		                                   -- pairs.
		sums !! ((sums !! index)) == index
			= [index, sums !! index]
	| otherwise = []
	
find_amicable_pairs upper =
	let
		sums = proper_divisor_sums upper
	in
		filter (/= []) $ map (get_partner sums) [0..upper]
		
sum_amicable_pairs upper =
	sum $ flatten $ find_amicable_pairs upper
	
challenge_021_naive =
	sum_amicable_pairs 10000
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_021_naive)))
