{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Text.Read (readMaybe)
import Data.Functor

import Log

{- Exercise 1 -}
-- | Parse(serialize) one entry of log string to LogMessage.
-- >>> parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- True
-- >>> parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- True
-- >>> parseMessage "This is not in the right format" == Unknown "This is not in the right format"
-- True
parseMessage :: String -> LogMessage
parseMessage str =
  case parseMsg (split ' ' str) of
    Just msg -> msg
    Nothing  -> Unknown str
  where
    parseTimeStamp = readMaybe
    parseLevel     = readMaybe
    -- I'm trying to use different form to understand this.
    parseMsg ("I":t:rest) =
      fmap (\ts -> LogMessage Info ts (unwords rest)) (parseTimeStamp t)
    parseMsg ("W":t:rest) =
      LogMessage Warning <$> (parseTimeStamp t) <*> Just (unwords rest)
    parseMsg ("E":lvl:t:rest) =
      let message = Error <$> parseLevel lvl in
      LogMessage <$> message <*> (parseTimeStamp t) <*> pure (unwords rest)
    parseMsg _ = Nothing

split :: Char -> String -> [String]
split ch = wordBy (==ch)

wordBy :: (Char -> Bool) -> String -> [String]
wordBy _ "" = []
wordBy isDelim str =
  let (h, t) = break isDelim str
  in h : wordBy isDelim (dropWhile isDelim t)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Doctest is less effective when it comes to IO,
-- I add some functions to verify the results.
test1 :: IO ()
test1 = do
  logs <- Log.testParse parse 10 "data/error.log"
  sequence_ $ map print logs


{- Exercise 2 -}
-- Insert by binary order, which is not balanced.
-- | I'm lazy to write tests.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert logMsg Leaf   = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ time1 _) (Node l curr@(LogMessage _ time2 _) r)
  | time1 < time2 = Node (insert logMsg l) curr r
  | otherwise     = Node l curr (insert logMsg r)
insert _ (Node _ (Unknown _) _) = error "MessageTree: contains Unknown logs."


{- Exercise 3 -}
-- Build sorted message tree with LogMessages.
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

testSamples :: IO [LogMessage]
testSamples = Log.testParse parse 100 "data/sample.log"

test3 :: IO ()
test3 = build <$> testSamples >>= print


{- Exercise 4 -}
-- Traverse meanwhile accumulating by in order.
inOrder :: MessageTree -> [LogMessage]
inOrder (Node l c r) = inOrder l ++ [c] ++ inOrder r
inOrder Leaf         = []

test4 :: IO ()
test4 = build <$> testSamples <&> inOrder >>= printListElm
  where printListElm = sequence_ . map print


{- Exercise 5 -}
messageLogMessage :: LogMessage -> String
messageLogMessage (LogMessage _ _ msg) = msg
messageLogMessage (Unknown msg)        = msg

orderByTime :: [LogMessage] -> [LogMessage]
orderByTime = inOrder . build

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageLogMessage . filter isRelavant . orderByTime

isRelavant :: LogMessage -> Bool
isRelavant (LogMessage (Error level) _ _) = level >= 50
isRelavant _                              = False

test5 :: IO ()
test5 =
  testWhatWentWrong parse whatWentWrong "data/sample.log"
  >>= sequence_ . map print


{- Exercise 6 -}
-- findInLogBy :: (String -> Bool) -> IO ()
-- findInLogBy p =
--   readFile "data/error.log" <&> parse
--   <&> orderByTime
--   <&> filter (p . messageLogMessage)
--   >>= sequence_ . map print

-- findEgotisticalHacker :: IO ()
-- findEgotisticalHacker = findInLogBy cond
--   where cond msg = msg `contains` " I " ||
--                    msg `contains` ".I"

-- contains :: String -> String -> Bool
-- contains str (c:cs) =
--   case dropWhile (/=c) str of
--     (_:rest) -> rest /= "" && rest `startWith` cs
--     ""       -> False
-- contains _ "" = True

-- startWith :: String -> String -> Bool
-- startWith (s:ss) (p:ps) = s == p && ss `startWith` ps
-- startWith "" (_:_)  = False
-- startWith _ ""  = True

-- test6 :: IO ()
-- test6 = findEgotisticalHacker


test :: IO ()
test = sequence_ $ [test1, test3, test4, test5]
