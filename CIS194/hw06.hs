{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Function
import Numeric

{- Exercise 1 -}
-- |
-- >>> fib 5
-- 5
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- -- Another expression which is not intuitive.
-- fib n = ((+) `on` fib) (n-1) (n-2)

-- |
-- >>> take 8 fibs1
-- [0,1,1,2,3,5,8,13]
fibs1 :: [Integer]
fibs1 = map fib [0..]


{- Exercise 2 -}
-- | O(n) to generate n elements in fibonacci sequence.
-- >>> take 10 fibs2 == take 10 fibs1
-- True
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)


{- Exercise 3 -}
-- Mandatory infinite data structure.
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = ("Stream "++) . (++"...") . show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t


streamFromList :: [a] -> Stream a
streamFromList = foldr (\e acc -> Cons e acc) (Cons undefined undefined)

test3 :: IO ()
test3 = putStrLn . show . streamFromList $ fibs2


{- Exercise 4 -}
-- |
-- >>> (eqInits 5) (streamToList . streamRepeat $ 1) (repeat 1)
-- True
streamRepeat :: a -> Stream a
streamRepeat n = n `Cons` streamRepeat n
-- I like this form because it's more like `repeat x = x : repeat x`.

-- |
-- >>> (eqInits 5) (streamToList . streamMap (+1) . streamRepeat $ 1) (repeat 2)
-- True
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = f h `Cons` streamMap f t

-- |
-- >>> (eqInits 5) (streamToList . streamFromSeed fib $ 0) (iterate fib 0)
-- True
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = s `Cons` streamFromSeed f (f s)
-- In Data.List there's an alike function `iterate`.

eqInits :: Eq a => Int -> ([a] -> [a] -> Bool)
eqInits = on (==) . take


{- Exercise 5 -}
nats :: Stream Integer
nats = streamFromSeed (+1) 0


streamDrop :: Integer -> Stream a -> Stream a
streamDrop n s@(Cons _ t)
  | n <= 0    = s
  | otherwise = streamDrop (n-1) t

positives :: Stream Integer
positives = streamDrop 1 nats


instance Functor Stream where
  fmap = streamMap

instance Foldable Stream where
  foldr f z (Cons h t) = foldr f (h `f` z) t


-- |
-- >>> (eqInits 10) (streamToList ruler) [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
-- True
ruler :: Stream Integer
ruler = fmap f positives
  where
    -- :D, although not that faster than do it in lower level.
    f = fromIntegral . length . takeWhile (=='0') . reverse . showBinary
    showBinary n = showBin n ""


streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as') bs = Cons a (streamInterleave bs as')
-- -- This below version is bad implementaion because it takes to elements each time.
-- -- With this version the below `ruler'` will computes to infinite recursion.
-- streamInterleave (Cons a as') (Cons b bs') = Cons a (Cons b streamInterleave as' bs')

streamTakeList :: Int -> Stream a -> [a]
streamTakeList n = take n . streamToList

-- | A generative strategy.
-- | Observing that (streamTakeList 3), (streamTakeList 7), ...,
-- | (streamTake 2^n + 1) are symetric.
-- >>> (eqInits 20) (streamToList ruler') (streamToList ruler)
-- True
ruler' :: Stream Integer
ruler' = gen 0
  where gen n = streamInterleave (streamRepeat n) (gen (n+1))


{- Exercise 6 -}
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

mulX :: Stream Integer -> Stream Integer
mulX = Cons 0

mulN :: Integer -> Stream Integer -> Stream Integer
mulN n = fmap (*n)

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  (+) (Cons a0 aas) (Cons b0 bbs) = Cons (a0 + b0) (aas + bbs)
  (*) (Cons a0 aas) bs@(Cons b0 bbs) = h + t
    where h = fromInteger (a0 * b0)
          t = mulX $ (mulN a0 bbs) + (aas * bs)


test6 :: IO ()
test6 = sequence_ . map putStrLn . map show $
  [x^4, (1 + x)^5, (x^2 + x + 3) * (x - 5)]


instance Fractional (Stream Integer) where
  (/) (Cons a0 as') (Cons b0 bs') = result
    where result = h + t
          h = fromInteger (a0 `div` b0)
          t = mulX . (mulN (1 `div` b0)) $ (as' - result * bs')

-- |
-- >>> (eqInits 100) (streamToList fibs3) fibs2
-- True
fibs3 :: Stream Integer
fibs3 = x / (1-x-x^2)


{- Exercise 7 -}
data Matrix = Matrix Integer Integer Integer Integer
            deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
    = Matrix (a11*b11+a12*b21) (a11*b12+a12*b22)
             (a21*b11+a22*b21) (a21*b12+a22*b22)

f1 :: Matrix
f1 = Matrix 1 1 1 0

-- |
-- >>> (eqInits 100) (map fib4 [0..]) fibs2
-- True
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = a11 (f1 ^ (n-1))
  where a11 (Matrix v _ _ _) = v


test7 :: IO ()
test7 = print $ fib4 (10^6)


test :: IO ()
test = sequence_ [test3, test6, test7]
