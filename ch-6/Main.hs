all3 :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)

all3 p = and . map p
all4 p = not . any (not . p)

all5 p xs = foldl (&&) True (map p xs)

all6 p = foldr (&&) True . map p

any1 p xs = length (filter p xs) > 0
any2 p = or . map p

any3 p = not . null . dropWhile (not . p)

any4 p xs = not (all (\x -> not (p x)) xs)

any5 p xs = foldr (\ x acc -> (p x) || acc) False xs

dw _ [] = []
dw p (x:xs)
	| p x = dw p xs
	| otherwise = x : xs


map1 f = foldr (\ x xs -> xs ++ [f x]) []
map2 f = foldl (\ xs x -> xs ++ [f x]) []

--filter1 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []
filter2 p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0
-- d2i = foldl (\ x y -> x + 10 * y) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sumsqreven = compose [sum, map (^2), filter even]

cur :: ((a, b) -> c) -> a -> b -> c
cur f = \ (x, y) -> f x y

unfold p h t x
 | p x = []
 | otherwise = h x : unfold p h t (t x)

iterate f = unfold (const False) id f
