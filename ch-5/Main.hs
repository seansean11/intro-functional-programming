sum100 = sum [x^2 | x <- [1..100]]

-- replicate n a = [a | _ <- [1..a]]

pyths n 
	= [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],
		x ^ 2 + y ^ 2 == z ^ 2]

factors n = 
	[x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum (init (factors num)) == num

test = concat[[[(x,y)] | y <- [4,5,6]] | x <- [1,2,3]]

scalarproduct xs ys = sum [x * y | x <- xs, y <- ys]
scalarproduc2 xs ys = sum [x * y | (x,y) <- xs `zip` ys]

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]


t1 [] = []
t1 (xs : xss) = xs ++ t1 xss

rep 0 _ = []
rep n x = x : rep (n-1) x

ex1 _ [] = False
ex1 x (y : ys)
	| x == y = True
	| otherwise = ex1 x ys

ex2 [] ys = ys
ex2 xs [] = xs
ex2 (x : xs) (y : ys)
	= if x <= y then x : ex2 xs (y : ys) else y : ex2 (x : xs) ys

halve xs = splitAt (length xs `div` 2) xs


ex3 [] = []
ex3 [x] = [x]
ex3 xs = ex2 (ex3 ys) (ex3 zs)
	where (ys, zs) = halve xs
