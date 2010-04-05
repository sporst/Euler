{-
	Project Euler Challenge 017:

	How many letters would be needed to write all the numbers in words from 1 to
	1000?
-}

ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

calculate_word number
	| number == 1000 = "onethousand"
	| number `mod` 100 == 0 = (ones !! (number `div` 100 - 1)) ++ "hundred"
	| number > 100 = (ones !! (number `div` 100 - 1)) ++ "hundredand" ++ (calculate_word (number `mod` 100))
	| number <= length ones = ones !! (number - 1)
	| number `mod` 10 == 0 = tens !! (number `div` 10 - 2)
	| otherwise = (tens !! (number `div` 10 - 2)) ++ calculate_word (number `mod` 10)
	
calculate_letters =
	map calculate_word [1..1000]
	
calculate_length =
	sum $ map length calculate_letters
	
challenge_017_naive =
	calculate_length
	
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_017_naive)))
