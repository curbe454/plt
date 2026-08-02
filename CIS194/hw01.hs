{-# OPTIONS_GHC -Wall #-}

{- Exercise 1 -}
-- | least significant digit
lsd :: Integer -> Integer
lsd n = n `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lsd n : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


{- Exercise 2 -}
double :: Integer -> Integer
double n = n * 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:(y:zs)) = double x : y : doubleEveryOther zs
doubleEveryOther xs = xs


{- Exercise 3 -}
sumDigits :: [Integer] -> Integer
sumDigits = sum . (concatMap toDigits)


{- Exercise 4 -}

-- |
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate n =
  checksum `mod` 10 == 0
  where
    checksum = sumDigits . doubleEveryOther . toDigits $ n


{- Exercise 5 -}
type Peg = String
type Move = (Peg, Peg)

-- |
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
-- >>> length (hanoi 15 "a" "b" "c")
-- 32767
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start target aux
  | n <= 0    = []
  | n == 1    = [(start, target)]
  | otherwise = concat
    [ hanoi (n-1) start aux target
    , hanoi 1 start target aux
    , hanoi (n-1) aux target start
    ]


{- Exercise 6 -}
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 n start aux1 aux2 target
--   | n <= 0    = []
--   | n <= 2    = hanoi 2 start aux1 target
--   | n == 3    = [ (start, aux1), (start, aux2), (start, target)
--                 , (aux2, target), (aux1, target) ]
--   | n == 4    = concat
--     [ hanoi4 (n-2) start aux1 target aux2
--     , hanoi 2 start aux1 target
--     , hanoi4 (n-2) aux2 start aux1 target
--     ]
--   | otherwise = concat
--     [ hanoi4 (n-3) start target aux2 aux1
--     , hanoi 3 start aux2 target
--     , hanoi4 (n-3) aux1 start aux2 target
--     ]

-- hanoi4a :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4a a n start aux1 aux2 target
--   -- but maybe the a should not be constant, or to say
--   -- a is related to n for optimized result
--   | n <= 0    = []
--   | n <= 2    = hanoi 2 start aux1 target
--   | n == 3    = [ (start, aux1), (start, aux2), (start, target)
--                 , (aux2, target), (aux1, target) ]
--   | n == 4    = concat
--     [ hanoi4a a (n-2) start aux1 target aux2
--     , hanoi 2 start aux1 target
--     , hanoi4a a (n-2) aux2 start aux1 target
--     ]
--   | otherwise = concat
--     [ hanoi4a a (n-a) start target aux2 aux1
--     , hanoi a start aux2 target
--     , hanoi4a a (n-a) aux1 start aux2 target
--     ]

-- h :: [Int]
-- h = map (\a -> length (hanoi4a a 15 "a" "b" "c" "d")) [1..15]
-- -- [20479,509,185,145,217,209,393,765,1533,3069,2065,4105,8197,16389,32767]
