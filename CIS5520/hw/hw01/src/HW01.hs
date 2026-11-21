module HW01 where

import Prelude hiding (all, concat, reverse, takeWhile, zip, (++))
import Data.Maybe (isNothing)

--------------------------------------------------------------------------------
-- Problem (Good Style)
--------------------------------------------------------------------------------

-- >>> abc True False True
-- True
-- >>> abc True False False
-- False
-- >>> abc False True True
-- False
abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)

-- >>> arithmetic ((1,2),3) ((4,5),6)
-- (-3,6,-3)
-- >>> arithmetic ((3,2),1) ((4,5),6)
-- (7,-14,7)
arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((p, q), s) =
  ( (b * s) - (c * q)
  , (c * p) - (a * s)
  , (a * q) - (b * p)
  )

-- >>> reverse [3,2,1]
-- [1,2,3]
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

-- >>> zip "abc" [True,False,True]
-- [('a',True),('b',False),('c',True)]
-- >>> zip [1,2] "a"
-- [(1,'a')]
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _           = []

--------------------------------------------------------------------------------
-- Problem (List recursion)
--------------------------------------------------------------------------------

-- Do not use any library functions in this problem.

-- | The 'minimumMaybe` function computes the mininum value of a
-- nonempty list. If the list is empty, it returns Nothing.
--
-- >>> minimumMaybe [2,1,3]
-- Just 1
-- >>> minimumMaybe []
-- Nothing
minimumMaybe :: [Int] -> Maybe Int
minimumMaybe []     = Nothing
minimumMaybe (x:xs) = minMaybe x (minimumMaybe xs)
  where minMaybe a b = case b of
          Nothing -> Just a
          Just b' -> Just (min a b')
-- minimumMaybe xs = Just . foldl1 min $ xs

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
-- >>> "World" `startsWith` "Hello World!"
-- False
-- >>> "Helo" `startsWith` "Hello World!"
-- False
startsWith :: String -> String -> Bool
startsWith (p:pat) (s:str) = p == s && startsWith pat str
startsWith (_p:_pat) "" = False
startsWith _ _          = True

-- | The 'isSubsequenceOf' function takes two strings and returns 'True'
-- when all characters in the first are contained within the second, in order.
--
-- >>> "Hello" `isSubsequenceOf` "Hello World!"
-- True
-- >>> "World" `isSubsequenceOf` "Hello World!"
-- True
-- >>> "Helo" `isSubsequenceOf` "Hello World!"
-- True
-- >>> "Wello" `isSubsequenceOf` "Hello World!"
-- False
isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf sub@(a:as) (b:bs) =
  if a == b then as `isSubsequenceOf` bs else sub `isSubsequenceOf` bs
isSubsequenceOf "" _     = True
isSubsequenceOf (_:_) "" = False

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
-- Note: You can use other functions that you have defined in this file in
-- your solution.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5
countSub :: String -> String -> Int
countSub pat str@(_:ss) = fromEnum (pat `startsWith` str) + countSub pat ss
countSub pat ""         = fromEnum $ pat == ""

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. You may assume that the input list is non-empty, and that each
-- of the sublists is also non-empty.
-- (i.e. we won't test your code on `transpose []` or `transpose [[]]`)
-- Note, this function should *not* have the same behavior as the library version
-- of transpose (i.e. the version of transpose from Data.List), which retains
-- extra elements in the output.
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- >>> transpose [[3,4,5]]
-- [[3],[4],[5]]
-- >>> transpose [[1,2],[3,4,5]]
-- [[1,3],[2,4]]
-- >>> transpose [[1,2],[3,4],[5,6]]
-- [[1,3,5],[2,4,6]]

-- (WARNING: this one is tricky!)
transpose :: [[a]] -> [[a]]
transpose []         = [[]]
transpose (row:[])   = map' (:[]) row
  where map' f (x:xs) = f x : map' f xs
        map' _ []     = []
transpose (row:rows) = zipWith' append (transpose [row]) (transpose rows)
  where append as bs = f (reverse as) bs
                where f (x:xs) ys = f xs (x:ys)
                      f [] ys     = ys
        zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
        zipWith' _f [] _         = []
        zipWith' _f _ []         = []

--------------------------------------------------------------------------------
-- Problem (Defining higher-order functions)
--------------------------------------------------------------------------------

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []
takeWhile _ []     = []

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3
-- >>> find odd [0,2,4,6]
-- Nothing
find :: (a -> Bool) -> [a] -> Maybe a
find p (x:xs) = if p x then Just x else find p xs
find _ []     = Nothing

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False
all :: (a -> Bool) -> [a] -> Bool
all p xs = isNothing $ find (== False) (map p xs)

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ [] _         = []
map2 _ _ []         = []

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes' . map f
  where catMaybes' []     = []
        catMaybes' (x:xs) = case x of
          Just v  -> v : catMaybes' xs
          Nothing -> catMaybes' xs

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

--------------------------------------------------------------------------------
-- Problem (map and foldr practice for lists)
--------------------------------------------------------------------------------

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
concat :: [[a]] -> [a]
concat = foldr (++) []
  where as ++ bs = foldr (:) bs as

-- | The 'startsWithHO' function takes two strings and returns 'True'
-- iff the first is a prefix of the second. This is the same as `startsWith` above
-- except this time you need to use `foldr` to define it.
--
-- >>> "Hello" `startsWithHO` "Hello World!"
-- True
--
-- >>> "Hello" `startsWithHO` "Wello Horld!"
-- False
--
-- >>> "Hello" `startsWithHO` "He"
-- False
startsWithHO :: String -> String -> Bool
-- startsWithHO pat str = foldr f True str
--   where f = foldr (\p1 p2 -> (\c1 -> \c2 -> p1 c1 && p2 c2)) (\_ -> True) (map (==) pat)
-- -- But this accumulates to (\c1 -> \c2 -> \c3 -> ... -> True), not match the type signature.

-- startsWithHO pat str = f str
--   where f = foldr (\c p -> (\(s:ss) -> s==c && p ss)) (\_nil -> True) pat
startsWithHO = foldr (\c p -> (\str -> case str of
                                 "" -> False
                                 (s:ss) -> s==c && p ss))
                     (\_nil -> True)

-- INTERLUDE: para

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x : xs) = f x xs (para f b xs)

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
--
-- >>> tails "abc"
-- ["abc","bc","c",""]
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- It is a natural fit to implement tails using para.
-- See if you can redefine the function above so that the test cases still pass.
--
-- >>> tails' "abc"
-- ["abc","bc","c",""]
tails' :: [a] -> [[a]]
tails' = (\f s -> s : f s) $ para (\_ xs t -> xs:t) []

-- | The 'countSubHO' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSubHO "aa" "aaa"
-- 2
-- >>> countSubHO "" "aaac"
-- 5
countSubHO :: String -> String -> Int
countSubHO pat = (\f s -> f s + fromEnum (pat `startsWithHO` s)) $
  para (\_ xs t -> fromEnum (pat `startsWithHO` xs) + t) 0
