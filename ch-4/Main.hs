halve xs =  splitAt (length xs `div` 2) xs
halve1 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
	where n = length xs
halve2 xs = splitAt (div (length xs) 2) xs
halve3 xs = (take n xs, drop n xs)
	where n = length xs `div` 2

safetail [] = []
safetail (_ : xs) = xs

safetail1 xs
	| null xs = []
	| otherwise = tail xs

safetail3 [] = []
safetail3 xs = tail xs

safetail4 [x] = [x]
safetail4 (_ : xs) = xs

safetail5
	= \ xs ->
		case xs of
			[] -> []
			(_ : xs) -> xs

funct x xs = take (x + 1) xs ++ drop x xs

e4 (x, y) = x

e6 x y = x * y

e7 (x, y) = (y, x)

e8 x y = (y, x)

e9 [x, y] = (x, True)

e10 (x, y) = [x, y]

e13 x y = x + y * y
