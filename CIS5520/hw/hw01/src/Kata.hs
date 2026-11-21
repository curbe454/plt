--------------------------------------------------------------------------------
-- Problem (Data Munging Kata)
--------------------------------------------------------------------------------

module Kata where

-- libraries

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Text.Read (readMaybe)

-- Part One: Weather
--
-- Your job is to write a program to output the day number (column one) with
-- the smallest temperature spread (the maximum temperature is the second
-- column, the minimum the third column). If there are multiple days with the
-- same smallest spread, your program should return the first one.

-- Utils {{{

-- |
-- >>> dropUntil (==3) [1..5]
-- [3,4,5]
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p = dropWhile (not . p)

-- |
-- >>> dropUntil_ (==3) [1..5]
-- [4,5]
dropUntil_ :: (a -> Bool) -> [a] -> [a]
dropUntil_ p = drop 1 . dropUntil p

-- |
-- >>> dropUntilEnd_ (==3) [1..5]
-- >> [1,2]
dropUntilEnd_ :: (a -> Bool) -> [a] -> [a]
dropUntilEnd_ p = reverse . dropUntil_ p . reverse

iterateNth :: Integer -> (a -> a) -> a -> a
iterateNth n f x
  | n <= 0    = x
  | otherwise = iterateNth (n-1) f (f x)

extremeMaybe :: (a -> a -> a) -> [a] -> Maybe a
extremeMaybe cmp (x:xs) = Just $ foldr cmp x xs
extremeMaybe _ []       = Nothing

-- |
-- >>> splitBy (==' ') "foo bar baz"
-- ["foo","bar","baz"]
splitBy :: (Char -> Bool) -> String -> [String]
splitBy p s = case span (not . p) s of
                ("","") -> []
                (h,t)  -> h : splitBy p (drop 1 t)

-- }}}


-- | Extract table from dat files by detecting "=========..." lines.
-- >>> tableLinesFrom "foo\n===\nbar\n===\nbaz\n"
-- ["===","bar","==="]
-- >>> tableLinesFrom "===\nbar\n==="
-- ["===","bar","==="]
tableLinesFrom :: String -> [String]
tableLinesFrom = List.dropWhileEnd p . dropWhile p . lines
  where p = \str -> not ("===" `List.isPrefixOf` str)

-- |
-- >>> tableEntries ["===","","1 2 3","===","foo","===","==="]
-- ["1 2 3"]
tableEntries :: [String] -> [String]
tableEntries = dropSumRow . dropHeadRow
  where dropHeadRow = dropUntil_ (== "")
        dropSumRow = iterateNth 3 (dropUntilEnd_ ("===" `List.isPrefixOf`))

type Day = Int
data Weather = Weather Day Int Int
               deriving (Eq, Show)

-- >>> parseWeather "1 2 3"
-- Just (Weather 1 2 3)
parseWeather :: String -> Maybe Weather
parseWeather = f . words
  where f (t1:t2:t3:_tokens) = Weather <$> readInt t1 <*> readInt t2 <*> readInt t3
        f _                  = Nothing

-- >>> weatherProgram "dat/jul24.dat"
-- "12"
-- >>> weatherProgram "dat/jul23.dat"
-- "16"

weather :: String -> Maybe String
weather = fmap show
  . fmap (\(Weather d _ _) -> d)
  . extremeMaybe minSpread
  . Maybe.catMaybes . map parseWeather
  . tableEntries . tableLinesFrom
  where minSpread w1@(Weather _ mx1 mn1) w2@(Weather _ mx2 mn2) =
          if mx1 - mn1 < mx2 - mn2 then w1 else w2

weatherProgram :: String -> IO String
weatherProgram file = do
  str <- readFile file
  return
    ( case weather str of
        Just result -> result
        Nothing -> "Cannot read file"
    )

-- | Use this function to parse Ints
readInt :: String -> Maybe Int
readInt = readMaybe

-- Part Two: Soccer League Table

-- The file dat/soccer22.dat contains the results from the English Premier
-- League for the 2022/2023 season. This data is taken from SkySports.The
-- columns labeled "W" and "L" contain the total number of wins and losses for
-- each team in that season (so Liverpool won 19 games against opponents and
-- lost nine). Write a program to find the name of the team with the smallest
-- (absolute) difference in "W" and "L". If there are multiple teams with the
-- smallest difference, your program should return the first one.

data Team = Team String Int Int
            deriving (Eq, Show)

parseTeam :: String -> Maybe Team
parseTeam = parse . splitBy (=='\t')
  where parse (_id:name:_:win:_:lose:_tokens) =
          Team <$> Just name <*> readInt win <*> readInt lose
        parse _ = Nothing

-- >>> soccerProgram "dat/soccer23.dat"
-- "West Ham United"

-- >>> sequence $ map soccerProgram $ map (\n -> List.concat ["dat/soccer", show n, ".dat"]) [18..23]

soccer :: String -> Maybe String
soccer = fmap (\(Team name _ _) -> name)
  . extremeMaybe f
  . Maybe.catMaybes . map parseTeam
  . tableEntries' . tableLines'
  where f t1@(Team _ w1 l1) t2@(Team _ w2 l2) =
          if abs(w1-l1) < abs(w2-l2) then t1 else t2
        tableLines' = takeWhile startWithDigit . dropUntil_ ("#" `List.isPrefixOf`) . lines
        tableEntries' = id
        startWithDigit (c:_) = Char.isDigit c
        startWithDigit []     = False

soccerProgram :: String -> IO String
soccerProgram file = do
  str <- readFile file
  return $ case soccer str of
    Just result -> result
    Nothing -> "Cannot read file"

-- Part Three: DRY Fusion

weather2 :: String -> Maybe String
weather2 = weather

soccer2 :: String -> Maybe String
soccer2 = soccer

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "Fill in your answer here"

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "Fill in your answer here"

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "Not always. So I did not refactor them."
