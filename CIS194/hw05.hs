{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -XFunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import Data.List
import qualified Data.Map as M

import ExprT
import Parser
import StackVM

{- Exercise 1 -}
-- |
-- >>> eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4))
-- 20
eval :: ExprT -> Integer
eval (ExprT.Lit n)   = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b


{- Exercise 2 -}
-- |
-- >>> parseExp ExprT.Lit ExprT.Add ExprT.Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- >>> evalStr "(2+3)*4"
-- Just 20
-- >>> evalStr "2+3*4"
-- Just 14
-- >>> evalStr "2+3*"
-- Nothing
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


{- Exercise 3 -}
-- -- I was about to write
-- class Expr e v where
-- lit :: v -> e
-- add, mul :: e -> e -> e
-- -- But this makes compilation error because the `add` add `mul` doesn't
-- -- use type v.
-- -- CIS194 should tell me more.
-- -- The below need `-XFunctionalDependencies` for GHC.

-- |
-- >>> mul' (add' (lit' 2) (lit' 3)) (lit' 4) :: ExprT
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr' e v | e -> v where
  lit' :: v -> e
  add', mul' :: e -> e -> e

instance Expr' ExprT Integer where
  lit' = ExprT.Lit
  add' = ExprT.Add
  mul' = ExprT.Mul

-- -- Here's another:
-- -- `-XTypeFamilies` is needed.
-- import Data.Kind (Type)

-- class Expr e where
--   type Val e :: Type
--   lit :: Val e -> e
--   add, mul :: e -> e -> e

-- instance Expr ExprT where
--   type Val ExprT = Integer
--   lit = Lit
--   add = Add
--   mul = Mul

reify :: ExprT -> ExprT
reify = id


{- Exercise 4 -}
-- It seems I did something not predicted to do in Exercise 3.
maxBy :: Ord a => (b -> a) -> b -> b -> a
maxBy f a b = f a `max` f b

newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7   = Mod7   Integer deriving (Show, Eq)

instance Ord MinMax where
  (<=) (MinMax a) (MinMax b) = a <= b
-- -- Or this:
-- newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)


class Expr e where
  lit :: Integer -> e
  add, mul :: e -> e -> e

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 *-4) + 5"

-- |
-- >>> testInteger
-- Just (-7)
-- >>> testBool
-- Just True
-- >>> testMM
-- Just (MinMax 5)
-- >>> testSat
-- Just (Mod7 0)
testInteger :: Maybe Integer
testInteger = testExp
testBool    :: Maybe Bool
testBool    = testExp
testMM      :: Maybe MinMax
testMM      = testExp
testSat     :: Maybe Mod7
testSat     = testExp


{- Exercise 5 -}
testStackVM :: Maybe Program
testStackVM = testExp

-- |
-- >>> stackVM <$> testStackVM
-- Just (Right (IVal (-7)))
instance Expr Program where
  lit = singleton . PushI
  add a b =  a ++ b ++ [StackVM.Add]
  mul a b =  a ++ b ++ [StackVM.Mul]

-- |
-- >>> compile "(3 *-4) + 5"
-- Just [PushI 3,PushI (-4),Mul,PushI 5,Add]
compile :: String -> Maybe Program
compile = parseExp lit add mul


{- Exercise 6 -}
-- -- It's blame to the description of homework. The below commented codes
-- -- are just for inspiration.
-- data VarExprT = VarLit String Integer
--               | VarAdd String VarExprT VarExprT
--               | VarMul String VarExprT VarExprT

-- instance Expr VarExprT where
--   lit = VarLit "anon"
--   add = VarAdd "anon"
--   mul = VarMul "anon"

-- instance HasVars VarExprT where
--   var s = VarLit s 0

class HasVars a where
  var :: String -> a

type Env = (M.Map String Integer)
type VarExprTNoEnv = M.Map String Integer -> Maybe Integer

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i     = \_env -> Just i
  -- This type signature is more obvious:
  add :: VarExprTNoEnv -> VarExprTNoEnv -> VarExprTNoEnv
  add ea eb = \env -> (+) <$> ea env <*> eb env
  mul ea eb = \env -> liftA2 (*) (ea env) (eb env)
  -- I'm learning by comparing these two forms.

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup


-- |
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expBy = expBy env
  where env :: Env = M.fromList vs
