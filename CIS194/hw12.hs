{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Functor

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

{- Exercise 1 -}
die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

{- Exercise 2 -}
newtype BattleDiff = BattleDiff { getBattleDiff :: (Int, Int) }
  deriving (Show, Eq)

instance Semigroup BattleDiff where
  BattleDiff (a1, d1) <> BattleDiff (a2, d2) = BattleDiff (a1+a2, d1+d2)

instance Monoid BattleDiff where
  mempty = BattleDiff (0, 0)

battleResult :: Battlefield -> BattleDiff -> Battlefield
battleResult (Battlefield a d) (BattleDiff (da,dd)) = Battlefield (a+da) (d+dd)

battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
  let (Battlefield an dn) = field
  let atk = min 3 . max 0 $ an-1
      dfn = min 2 dn
  let cannotDefend = BattleDiff (0, min 0 $ dfn-atk)
  battling <- combats <$> diePairs (min atk dfn)
  return $ battleResult field (cannotDefend <> battling)

  where diePairs n = liftA2 zip (sort <$> dieInts n) (sort <$> dieInts n)

combats :: [(Int, Int)] -> BattleDiff
combats = mconcat . map BattleDiff . map combat
  where combat (a, d) = if a > d then (0, -1) else (-1, 0)

dies :: Int -> Rand StdGen [DieValue]
dies n = sequence $ replicate n die

dieInts :: Int -> Rand StdGen [Int]
dieInts n = map unDV <$> dies n


test2 :: IO ()
test2 = do
  _ <- print =<< (evalRandIO . dieInts $ 5)
  result <- evalRandIO $ battle (Battlefield 2 5)
  print result


{- Exercise 3 -}
invade :: Battlefield -> Rand StdGen Battlefield
invade f
  | isInvadeDone f = return f
  | otherwise      = invade =<< battle f

-- -- This version makes a quirk bug that compute infinitely on test4.
-- invade f = head <$> fmap (dropWhile $ not . isInvadeDone) rounds
--   where
--     rounds :: Rand StdGen [Battlefield]
--     rounds = sequence $ iterate (>>= battle) (return f)

isInvadeDone :: Battlefield -> Bool
isInvadeDone (Battlefield a d) = d == 0 || a < 2


test3 :: IO ()
test3 = do
  result <- evalRandIO $ invade (Battlefield 2 2)
  print result


{- Exercise 4 -}
successProbN :: Int -> Battlefield -> Rand StdGen Double
successProbN n f = fmap fromInteger success <&> (/ fromIntegral n)
  where success = sum <$> map isSuccess <$> (sequence . map invade . replicate n $ f)
        isSuccess (Battlefield _ d) = if d==0 then 1 else 0

successProb :: Battlefield -> Rand StdGen Double
successProb = successProbN 1000

test4 :: IO ()
test4 = do
  result <- evalRandIO $ successProb (Battlefield 2 2)
  print result


test :: IO ()
test = sequence_ [test2,test3,test4]
