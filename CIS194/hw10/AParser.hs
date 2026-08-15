{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

type Parse a = String -> Maybe (a, String)

{- Exercise 1 -}
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (\inp -> first f <$> p inp)

first :: (a -> b) -> (a,c) -> (b,c)
first f (k, v) = (f k, v)


{- Exercise 2 -}
instance Applicative Parser where
  -- `pure` fabricates a thing out of air other than fail.
  pure a = Parser (\inp -> Just (a, inp))
  Parser pf <*> Parser px = Parser (\inp -> pf inp >>= ap px)
    where ap p (f, rest) = first f <$> p rest


{- Exercise 3 -}
-- |
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing
abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') (char 'b')

-- |
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing
abParser_ :: Parser ()
abParser_ = liftA2 (\_ _ -> ()) (char 'a') (char 'b')

-- |
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")
intPair :: Parser [Integer]
intPair = liftA3 (\x _ y -> [x,y]) posInt (char ' ') posInt


{- Exercise 4 -}
-- |
-- >>> runParser (char 'a' <|> char 'b') "b"
-- Just ('b',"")
instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser pa <|> Parser pb = Parser (\inp -> pa inp <|> pb inp)


{- Exercise 5 -}
-- |
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing
intOrUppercase :: Parser ()
intOrUppercase = abandon posInt <|> abandon (satisfy isUpper)

abandon :: Parser a -> Parser ()
abandon = fmap (const ())
