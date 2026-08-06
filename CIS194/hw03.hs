{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

-- I need point-free.

{- Exercise 1 -}
-- |
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True,False]
-- [[True,False],[False]]
-- >>> skips []
-- []
skips :: [a] -> [[a]]
-- skips l = reverse . snd $ foldl (\(n, r) _ -> (succ n, nths n l : r)) (0, []) l
-- -- I found a function to do map meanwhile accumulating:
skips l = snd $ mapAccumL (\n _ -> (succ n, nths n l)) 0 l

nths :: Int -> [a] -> [a]
nths _ [] = []
nths n xs =
  case drop n xs of
    []    -> []
    (c:r) -> c : nths n r

-- | Another version.
-- >>> nths' 0 "abcd"
-- "abcd"
-- >>> nths' 3 "abcdefg"
-- "cf"
nths' :: Int -> [a] -> [a]
-- The below `last` won't raise error. This is why partial functons exists.
nths' n
  | n <= 0    = id
  | otherwise = map last . chunksOfDrop n


-- Some of libraries will provide this functions such as Split.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = chunk : chunksOf n rest
  where (chunk, rest) = splitAt n xs

-- | Like `chunksOf`, but drop the last element when its length is not enough.
-- >>> chunksOfDrop 3 "abcd"
-- ["abc"]
chunksOfDrop :: Int -> [a] -> [[a]]
chunksOfDrop n = dropNotEnough . chunksOf n
  where dropNotEnough [] = []
        dropNotEnough xs
          | length (last xs) /= n = init xs
          | otherwise             = xs

--   Just a practice.
-- | Like `chunksOf`, but do something wht the last element when its length
-- | is not enough. You may want to pad it to ensure the length.
-- >>> chunksOfPad 3 (take 3 . cycle) "abcd"
-- ["abc","ddd"]
chunksOfPad :: Int -> ([a] -> [a]) -> [a] -> [[a]]
chunksOfPad n f = padLast . chunksOf n
  where padLast [] = []
        padLast xs =
          let l = last xs
          in if length l /= n then (init xs) ++ [f l] else xs


{- Exercise 2 -}
-- |
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:ys@(y:z:_)) = localMax x y z ++ localMaxima ys
  where localMax l c r = if c > l && c > r then [c] else []
-- localMaxima (_:_:[])       = []
localMaxima _              = []

-- | Another version
-- >>> localMaxima' [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima' [2,3,4,1,5]
-- [4]
-- >>> localMaxima' [1,2,3,4,5]
-- []
localMaxima' :: [Integer] -> [Integer]
localMaxima' xs@(_:ys@(_:zs@(_:_))) =
  concatMap localMax . interleave $ map (chunksOfDrop 3) [xs,ys,zs]
  where interleave = concat . transpose
        localMax [l,c,r] = if c > l && c > r then [c] else []
        localMax _       = error "localMaxima': unknown error."
localMaxima' _ = []


{- Exercise 3 -}
histogram :: [Integer] -> String
histogram = showThem . histogramDataFrom
  where showThem dat = unlines . transpose . showBars len $ dat
          where len = maximum dat

-- |
-- >>> map reverse . showBars 3 $ [1,2,3]
-- ["0=*  ","1=** ","2=***"]
showBars :: Int -> [Int] -> [String]
showBars len xs =
  map (\(i, x) -> showIt len i x) (zip [0..] xs)
  where showIt :: Int -> Int -> Int -> String
        showIt l i num =
          reverse $ show i ++ "=" ++ replicate num '*' ++ replicate (l-num) ' '

-- |
-- >>> histogramDataFrom [1,1,2,3,9]
-- [0,2,1,1,0,0,0,0,0,1]
histogramDataFrom :: [Integer] -> [Int]
histogramDataFrom dat = [ length (filter (==n) dat) | n <- [0..9] ]


test3 :: IO ()
test3 = putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]


test :: IO ()
test = sequence_ [test3]
