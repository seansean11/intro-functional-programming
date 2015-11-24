multiply[] = 1 
multiply (x:xs) = x * multiply xs

double x = x + x
quadrupal x = double (double x)
factorial n = product[1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
	where
		a = 10
		xs = [1,2,3,4,5]
