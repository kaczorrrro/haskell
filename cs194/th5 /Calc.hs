{-# LANGUAGE FlexibleInstances #-}
module Calc where
import qualified ExprT as E
import Parser
import StackVM
import Control.Exception
import qualified Data.Map as M



eval:: E.ExprT -> Integer
eval (E.Lit val) = val
eval (E.Add e0 e1) = eval e0 + eval e1
eval (E.Mul e0 e1) = eval e0 * eval e1


evalStr = parseExp E.Lit E.Add E.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id


instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit =  (>0)
    add = (||)
    mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min


newtype Mod7 = Mod7 Integer deriving (Eq, Ord, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Task 5

instance Expr Program where
    lit x = [PushI x]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

-- Task 6
data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul

instance HasVars VarExprT where
    var = Var 

type MapSI = M.Map String Integer

instance HasVars (MapSI -> Maybe Integer) where
    var = M.lookup 

instance Expr (MapSI -> Maybe Integer) where
  lit a _   = Just a
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m