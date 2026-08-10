{-# OPTIONS_GHC -Wall #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Show, Read, Eq, Ord, Num)

getScore :: Score -> Int
getScore (Score v) = v

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0


-- https://en.wikipedia.org/wiki/Scrabble_letter_distributions
score :: Char -> Score
score ch = Score . dispatch $ toUpper ch
  where dispatch c
          | c `elem` "EAIONRTLSU" =  1
          | c `elem` "DG"         =  2
          | c `elem` "BCMP"       =  3
          | c `elem` "FHVWY"      =  4
          | c `elem` "K"          =  5
          | c `elem` "JX"         =  8
          | c `elem` "QZ"         = 10
          | otherwise             =  0

scoreString :: String -> Score
scoreString = foldMap (\ch -> score ch)
