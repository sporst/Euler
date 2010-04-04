{-
	Project Euler Challenge 012:

	What is the value of the first triangle number to have over five hundred
	divisors?
-}

triangles =
	scanl (+) 1 [2..]
	
flatten f =
	foldl (++) [] f

divisors n =
	flatten [[k, n `div` k] | k <- [1.. floor $ sqrt $ fromIntegral n], n `mod` k == 0]

triangles_with_divisors k =
	head $ dropWhile (\x -> length (divisors x) < k) triangles
	
challenge_012_naive =
	triangles_with_divisors 500
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_012_naive)))
