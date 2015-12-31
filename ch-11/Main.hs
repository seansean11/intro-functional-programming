fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
--fib n = last (take n fibs)
fib n = fibs !! n

largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

treetest = Node (Node (Leaf) 1 (Leaf)) 3 (Node (Leaf) 2 (Leaf))

repeatTree :: a -> Tree a
repeatTree x = Node t x t
    where t = repeatTree x
