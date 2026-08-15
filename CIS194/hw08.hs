module Party where

import Data.Tree
import Data.List
import Data.Bifunctor

import Employee

{- Exercise 1 -}
instance Semigroup GuestList where
  (GL [] _) <> a = a
  a <> (GL [] _) = a
  (GL xs xv) <> (GL ys yv) = GL (xs++ys) (xv+yv)

singleton :: Employee -> GuestList
singleton x@(Emp _ fun) = GL [x] fun

glCons :: Employee -> GuestList -> GuestList
glCons x ys = Party.singleton x <> ys


instance Monoid GuestList where
  mempty = GL [] 0


moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


{- Exercise 2 -}
-- -- But the Data.Tree.Tree is the instance of Foldable.
-- instance Foldable Tree where
--   foldMap f (Node v ys) = f v <> concatMap (foldMap f) ys

-- |
-- >>> :{
--   treeFold (\t fs -> empFun t + sum fs) 0 testCompany
--   == foldr (\t -> ((empFun t) +)) 0 testCompany
-- :}
-- True
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node v ys) = v `f` map (treeFold f z) ys


{- Exercise 3 -}
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss poss = (withBoss, withoutBoss)
  where withBoss    = uncurry moreFun . bossCons . mconcat $ poss
        withoutBoss = foldMap (uncurry moreFun) $ poss
        bossCons (with, without) = (bossGlCons boss with, glCons boss without)

bossGlCons :: Employee -> GuestList -> GuestList
bossGlCons b@(Emp _ f) (GL gs _) = GL (b:gs) f


{- Exercise 4 -}
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel (mempty, mempty)

glFun :: GuestList -> Fun
glFun (GL _ f) = f


{- Exercise 5 -}
main = do
  company <- getCompany "data/company.txt"
  putStrLn $ format (maxFun company)
  where format (GL emps f) = unlines $
          ["Total fun: " <> show f] ++ sort (map empName emps)

getCompany :: FilePath -> IO (Tree Employee)
getCompany = fmap read . readFile
