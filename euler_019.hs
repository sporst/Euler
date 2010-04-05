{-
	Project Euler Challenge 019:

	How many Sundays fell on the first of the month during the twentieth century?
-}

days_per_month =
	[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	
days_per_month_leap =
	[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

is_leap year =
	year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
	
get_days_per_month year
	| is_leap year = days_per_month_leap
	| otherwise = days_per_month

get_days_per_months from to =
	-- Returns the number of days for each month between the start year and the
	-- end year.
	foldl (++) [] $ map get_days_per_month [from .. to]

sum_days_per_month from to =
	-- Calculates the number of days that have passed for each 1st of a month
	-- in the range since the start of the rage.
	scanl (+) 0 $ get_days_per_months from to

to_weekday day start_index =
	-- Takes the index of a day since the start range and calculates the day of
	-- the week for that day (0 is Sunday).
	(day + start_index) `mod` 7
	
calculate_first_sundays from to start_index =
	-- Calculates the number of sundays in the years of the given range. The
	-- weekday of the first day of the first year must be known.
	length $ filter (==0) $ map (flip to_weekday start_index) $ sum_days_per_month from to
	
challenge_019_naive =
	-- We know that Jan 1st 1900 is a Monday but our range actually starts in 1901
	-- so we have to subtract the Sundays of 1900.
	calculate_first_sundays 1900 2000 1 - calculate_first_sundays 1900 1900 1