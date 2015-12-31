last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_ : xs) = drop' (n -1) xs

--append :: [a] -> [a] -> [a]
--[] append ys = ys
--(x : xs) append ys = x : (xs append ys)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x : xs) = foldl' f (f v x) xs
