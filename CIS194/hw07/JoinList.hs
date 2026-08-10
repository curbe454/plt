{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


{- Exercise 1 -}
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = (<>)

instance Monoid m => Semigroup (JoinList m a) where
  Empty <> jl = jl
  jl <> Empty = jl
  x <> y     = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


{- Exercise 2 -}
-- |
-- >>> indexJ 5 . jlFromListS $ [1..10]
-- Just 6
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ v)   = Just v
indexJ i (Append j l r)
  | i >= sizeInt j    = Nothing
  | i < sizeInt lsize = indexJ i l
  | otherwise         = indexJ (i - (sizeInt . tag $ l)) r
  where lsize = tag l
indexJ _ _ = Nothing

sizeInt :: Sized a => a -> Int
sizeInt = getSize . size

-- |
-- >>> jlToList . dropJ 5 $ jlFromListS [1..10]
-- [6,7,8,9,10]
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl | i <= 0 = jl
dropJ i (Append j l r)
  | i >= sizeInt j     = Empty
  | i >= sizeInt lsize = dropJ (i - sizeInt lsize) r
  | otherwise          = case l of
      Empty         -> dropJ i r
      Single _ _    -> dropJ (i-1) r
      Append m _ lr -> dropJ (i - sizeInt m) lr
  where lsize = tag l
dropJ _ _ = Empty

-- |
-- >>> jlToList . takeJ 5 $ jlFromListS [1..10]
-- [1,2,3,4,5]
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl@(Single _ _)
  | i >= 1    = jl
  | otherwise = Empty
takeJ i jl@(Append j l r)
  | i >= sizeInt j     = jl
  | i >= sizeInt lsize = l +++ takeJ (i - sizeInt lsize) r
  | otherwise          = takeJ i l
  where lsize = tag l
takeJ _ _ = Empty



(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:__) !!? 0    = Just x
(_:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlFromList :: Monoid m => (a -> m) -> [a] -> JoinList m a
jlFromList _ []           = Empty
jlFromList f [x]          = Single (f x) x
jlFromList f (x:xs@(_:_)) = h <> t
  where h = Single (f x) x
        t = jlFromList f xs

jlFromListS :: [a] -> JoinList Size a
jlFromListS = jlFromList (\_ -> mempty + 1)


{- Exercise 3 -}
-- |
-- >>> scoreLine "hello world!"
-- Append (Score 17) (Single (Score 8) "hello") (Single (Score 9) "world!")
-- >>> scoreLine "yay" +++ scoreLine "haskell!"
-- Append (Score 23) (Single (Score 9) "yay") (Single (Score 14) "haskell!")
scoreLine :: String -> JoinList Score String
scoreLine = jlFromList scoreString . words


{- Exercise 4 -}
instance Buffer (JoinList (Score, Size) String) where
  toString   = unlines . jlToList
  fromString = jlFromList (\w -> (scoreString w, mempty + 1)) . lines
  line = indexJ
  replaceLine i l jl = h <> t
    where (h', t) = splitAtJ i jl
          h = takeJ (i-1) h' <> fromString l
  numLines = getSize . snd . tag
  value    = getScore . fst . tag

-- Implementing `dropJ` and `takeJ` from `splitAtJ` would be
-- better in performance.
-- |
-- >>> let (h, t) = splitAtJ 5 $ jlFromListS [1..10] in map jlToList [h,t]
-- [[1,2,3,4,5],[6,7,8,9,10]]
splitAtJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> (JoinList b a, JoinList b a)
splitAtJ i jl = (takeJ i jl, dropJ i jl)

initBuffer :: JoinList (Score, Size) String
initBuffer = fromString $ unlines
                [ "This buffer is for notes you don't want to save, and for"
                , "evaluation of steam valve coefficients."
                , "To load a different file, type the character L followed"
                , "by the name of the file."
                ]

main :: IO ()
main = runEditor editor $ initBuffer


{- Just a practice. -}
newtype JoinListM m a = JLM { getJLM :: JoinList a m }
  deriving (Show, Eq)

instance Foldable (JoinList m) where
  foldMap _ Empty          = mempty
  foldMap f (Single _ v)   = f v
  foldMap f (Append _ x y) = foldMap f x <> foldMap f y

instance Foldable (JoinListM a) where
  foldMap _ (JLM Empty)          = mempty
  foldMap f (JLM (Single m _))   = f m
  foldMap f (JLM (Append m x y)) =
    f m <> foldMap f (JLM x) <> foldMap f (JLM y)
