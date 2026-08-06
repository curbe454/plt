{-# OPTIONS_GHC -Wall #-}

import Data.List

( |> ) :: a -> (a -> b) -> b
( |> ) = flip ( $ )

{- Exercise 1 -}
fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun1'' :: [Integer] -> Integer
fun1'' xs = xs |> filter even |> map ((-) 2) |> product

-- | Sum up all evens in a Collatz sequence.
-- >>> fun2' 4
-- 6
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate collatz
  where collatz n
          | odd n     = 3*n + 1
          | otherwise = n `div` 2


{- Exercise 2 -}
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf

insertBalanced :: a  -> Tree a -> Tree a
insertBalanced x Leaf = Node 0 Leaf x Leaf
insertBalanced x (Node h l v r)
  | height l < height r = updateHeight (Node h (insertBalanced x l) v r)
  | otherwise           = updateHeight (Node h l v (insertBalanced x r))
  where updateHeight Leaf = Leaf
        updateHeight (Node _ ll vv rr) =
          Node (height ll `max` height rr + 1) ll vv rr
        height Leaf           = -1
        height (Node hh _ _ _) = hh

test2 :: IO ()
test2 = putStr . formatTree $ foldTree "ABCDEFGHIJ"
  where
    formatTree :: Show a => Tree a -> String
    formatTree Leaf = ""
    formatTree (Node 0 _ v _) = "[0 " ++ show v ++ "]\n"
    formatTree (Node h l v r) = replicate (fromInteger (2*h)) ' ' ++ "[" ++ show h ++ " " ++ show v ++ "]\n" ++ formatTree l ++ formatTree r


{- Exercise 3 -}
-- |
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False
xor :: [Bool] -> Bool
xor = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc = foldr (flip f) acc . reverse

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f z xs = foldr (\x k z' -> k (f z' x)) id xs z
-- :: (a -> b) -> b -> b -> [a] -> b
-- :: (a -> (c -> c)) -> (c -> c) -> (c -> c) -> [a] -> (c -> c)
-- :: (a -> (? -> b) -> [a] -> b) -> b -> b -> [a] -> b
-- TODO: this means recusive type.


{- Exercise 4 -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = generate $ [1..n] \\ sieve
  where generate = map (\k -> 2*k + 1)
        sieve = takeWhile (<=n)
               [ i + j + 2*i*j | i <- [1..], j <- [1..], 1 <= i, i <= j ]


test4 :: IO ()
test4 = print $ sieveSundaram 100


test :: IO ()
test = sequence_ [test2]
