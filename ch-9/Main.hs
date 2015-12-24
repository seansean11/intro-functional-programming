module Main where
import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

zero,one,two,three :: Nat
zero = Zero
one = Succ zero
two = Succ one
three = Succ two

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

natToInteger1 :: Nat -> Integer
natToInteger1 (Succ n) = natToInteger n + 1
natToInteger1 Zero = 0

--natToInteger2 :: Nat -> Integer
--natToInteger2 n = natToInteger n

natToInteger3 :: Nat -> Integer
natToInteger3 (Succ n) = 1 + natToInteger n
natToInteger3 Zero = 0

natToInteger4 :: Nat -> Integer
natToInteger4 Zero = 1
natToInteger4 (Succ n) = (1 + natToInteger n) - 1

natToInteger5 :: Nat -> Integer
natToInteger5 = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger6 :: Nat -> Integer
natToInteger6 = \ n -> genericLength [c | c <- show n, c == 'S']

--natToInteger7 :: Nat -> Integer
--natToInteger7 =i \ n -> length [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Succ Zero
integerToNat1 n = (Succ (integerToNat1 n))

--integerToNat2 :: Integer -> Nat
--integerToNat2 n
--    = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat3 :: Integer -> Nat
integerToNat3 n = integerToNat3 n

integerToNat4 :: Integer -> Nat
integerToNat4 (n+1) = Succ (integerToNat4 n)
integerToNat4 0 = Zero

integerToNat5 :: Integer -> Nat
integerToNat5 (n+1) = let m = integerToNat5 n in Succ m
integerToNat5 0 = Zero

--integerToNat6 :: Integer -> Nat
--integerToNat6 = head . m
--    where {
--          ; m 0 = [0]
--          ; m (n+1) = [sum [x | x <- (1 : m n)]]
--          }

--integerToNat7 :: Integer -> Nat
--integerToNat7 = \ n -> genericLength [c | c <- show n, isDigit c]

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add n m)

add1 :: Nat -> Nat -> Nat
add1 (Succ m) n = Succ (add1 n m)
add1 Zero n = n

--add2 :: Nat -> Nat -> Nat
--add2 Zero n = Zero
--add2 (Succ m) n = Succ (add2 m n)

--add3 :: Nat -> Nat -> Nat
--add3 (Succ m) n = Succ (add3 m n)
--add3 Zero n = Zero

--add4 :: Nat -> Nat -> Nat
--add4 n Zero = Zero
--add4 n (Succ m) = Succ (add n m)

--add5 :: Nat -> Nat -> Nat
--add5 n Zero = Zero
--add5 n (Succ m) = Succ (add5 n m)

add6 :: Nat -> Nat -> Nat
add6 n Zero = n
add6 n (Succ m) = Succ (add6 m n)

add7 :: Nat -> Nat -> Nat
add7 n (Succ m) = Succ (add7 m n)
add7 n Zero = n


mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

--data Ordering = LT
--              | EQ
--              | GT

--compare :: (Ord a) => a -> a -> Ordering
--compare m n | m == n = EQ
--            | m < n = LT
--            | m > n = GT

--data Tree = Leaf Integer
--          | Node Tree Integer Tree


treetest = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))
treetest1 = Node (Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 5)) (Leaf 4)) (Node (Leaf 6) (Leaf 9))


--occurs :: Integer -> Tree -> Bool
--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--    = case compare m n of
--        LT -> occurs m l
--        EQ -> True
--        GT -> occurs m r

--occurs1 :: Integer -> Tree -> Bool
--occurs1 m (Leaf n) = m == n
--occurs1 m (Node l n r)
--    | m == n = True
--    | m < n = occurs1 m l
--    | otherwise = occurs1 m r


data Tree = Leaf Integer
          | Node Tree Tree

balanced :: Tree -> Bool
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r)
    = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

--balanced1 :: Tree -> Bool
--balanced1 (Leaf _) = True
--balanced1 (Node l r) = abs (leaves l + leaves r) <= 1

balance :: [Integer] -> Tree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs

--balance1 :: [Integer] -> Tree
--halve1 xs = splitAt (length xs `div` 2) xs
--balance1 x = Leaf x
--balance1 xs = Node (balance1 ys) (balance1 zs)
--    where (ys, zs) = halve1 xs

--balance2 :: [Integer] -> Tree
--halve2 xs = splitAt (length xs `div` 2) xs
--balance2 [x] = Leaf x
--balance2 xs = Node ys zs
--    where (ys, zs) = balance2 (halve2 xs)

--balance3 :: [Integer] -> Tree
--halve3 xs = splitAt (length xs / 2) xs
--balance3 [x] = Leaf x
--balance3 xs = Node (balance3 ys) (balance3 zs)
--    where (ys, zs) = halve3 xs
