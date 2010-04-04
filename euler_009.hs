{-
	Project Euler Challenge 009:

	Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
-}

is_triple x y z = x^2 + y^2 == z^2

find_triples sum =
    -- Instead of checking whether the sum of the triple is 1000, we generate
    -- the triples in a way that ensures that the sum is always 1000. This
    -- saves *a lot* of time.
	[(x,y,z) |
		x <- [sum,sum-1..1],
		y <- [sum-x,sum-x-1..1],
		z <- [sum-x-y],
		x > y,
		is_triple x y z]

challenge_009_naive sum =
	let 
		(x, y, z) = head $ find_triples sum
	in
		x * y * z
		
main = do
	putStrLn ("Naive solution: " ++ (show (challenge_009_naive 1000)))
