import System.IO
import Data.Char

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             guess word

sgetLine :: IO String
sgetLine =  do x <- getCh
               if x == '\n' then
                  do putChar x
                     return []
               else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

guess :: String -> IO ()
guess word =
   do putStr "> "
      xs <- getLine
      if xs == word then
         putStrLn "You got it!"
      else
         do putStrLn (diff word xs)
            guess word

diff :: String -> String -> String
diff xs ys =
   [if elem x ys then x else '-' | x <- xs]

printname :: IO ()
printname =  do putStrLn "Enter your first name: "
                firstname <- getLine
                putStrLn "Enter your last name: "
                lastname <- getLine
                putStr "Your full name is: " >> putStr firstname >> putStr " " >> putStrLn lastname

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

-- getLine'a = geta ""
--geta :: String -> IO String
--geta xs = do x <- getChar
--             case x of
--                 ' ' -> putStr xs
--                 '\n' -> putStr xs
--                 _ -> geta (xs ++ [x])



--sequence_'1 :: Monad m => [m a] -> m ()
--sequence_'1 [] = return []
--sequence_'1 (m : ms) = m >> \ _ -> sequence_'1 ms

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

--sequence_'3 :: Monad m => [m a] -> m ()
--sequence_'3 ms = foldl (>>) (return ()) ms

sequence_'4 :: Monad m => [m a] -> m ()
sequence_'4 [] = return ()
sequence_'4 (m : ms) = m >> sequence_'4 ms

sequence_'5 :: Monad m => [m a] -> m ()
sequence_'5 [] = return ()
sequence_'5 (m : ms) = m >>= \ _ -> sequence_'5 ms

--sequence_'6 :: Monad m => [m a] -> m ()
--sequence_'6 ms = foldr (>>=) (return ()) ms

sequence_'7 :: Monad m => [m a] -> m ()
sequence_'7 ms = foldr (>>) (return ()) ms

--sequence_'8 :: Monad m => [m a] -> m ()
--sequence_'8 ms = foldr (>>) (return []) ms

--mapM'1 :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM'1 f as = sequence' (map f as)

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms)
    = do a <- m
         as <- sequence' ms
         return (a : as)

mapM'2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'2 f [] = return []
mapM'2 f (a : as)
    = f a >>= \ b -> mapM'2 f as >>= \ bs -> return (b : bs)

--mapM'3 :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM'3 f as = sequence_' (map f as)

--mapM'1 :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM'1 f as = sequence' (map f as)

--mapM'4 :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM'4 f [] = return []
--mapM'4 f (a : as)
--    = f a >> \ b -> mapM'4 f as >> \ bs -> return (b : bs)

--mapM'5 :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM'5 f [] = return []
--mapM'5 f (a : as) = 
--    do 
--        f a -> b
--        mapM'5 f as -> bs
--        return (b : bs)

mapM'6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'6 f [] = return []
mapM'6 f (a : as)
    = do b <- f a
         bs <- mapM'6 f as
         return (b : bs)

mapM'7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'7 f [] = return []
mapM'7 f (a : as)
    = f a >>=
        \ b ->
          do bs <- mapM'7 f as
             return (b : bs)

mapM'8 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'8 f [] = return []
mapM'8 f (a : as)
    = f a >>=
        \ b ->
          do bs <- mapM'8 f as
             return (bs ++ [b])


filterM'1 _ [] = return []
filterM'1 p (x : xs)
    = do flag <- p x
         ys <- filterM'1 p xs
         return (x : ys)
    
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b0 ls = step b0 ls where
    step b []     = return b
    step b (a:as) = f a =<< step b as

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m
    = do x <- m
         return (f x)
-- foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,2..10]) >>= \r -> putStrLn r

--liftM2 :: Monad m => (a -> b) -> m a -> m b
--liftM2 f m = m >>= \ a -> f a

liftM3 :: Monad m => (a -> b) -> m a -> m b
liftM3 f m = m >>= \ a -> return (f a)

--liftM4 :: Monad m => (a -> b) -> m a -> m b
--liftM4 f m = return (f m)

liftM5 :: Monad m => (a -> b) -> m a -> m b
liftM5 f m = m >>= \ a -> m >>= \ b -> return (f a)

--liftM6 :: Monad m => (a -> b) -> m a -> m b
--liftM6 f m = mapM f [m]

-- liftM7 :: Monad m => (a -> b) -> m a -> m b
-- liftM7 f m = m >> \ a -> return (f a)
