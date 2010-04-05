import Char
import List
import Monad

getLines = liftM lines . readFile

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
	| f x = splitOn f xs
	| otherwise = let (h,t) = break f l in h:(splitOn f t)
  
calculate_name_score name =
	-- The weight of a letter is its position in the alphabet starting with 1.
	sum $ map ((+(-ord 'A' + 1)) .ord) name
	
calculate_score names =
	let
		name_list = sort $ splitOn (==',') $ [c | c <- names, c /= '"']
	in
		zipWith (*) [1..] $ map calculate_name_score name_list

calculate_total_score names =
	sum $ calculate_score names
		
main = do
	names <- getLines "euler_022.in"
	putStrLn ("Naive solution: " ++ (show (calculate_total_score $ head names)))
