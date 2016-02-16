{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import Parser
import StackVM
import qualified Data.Map as M
import qualified ExprT as E
import Control.Monad

parseExpr :: Expr a => String -> Maybe a
parseExpr = parseExp lit add mul


class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a


instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>0)
  mul = (&&)
  add = (||)


newtype MinMax = MinMax Integer deriving (Eq, Show)


instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7


-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"

-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7


instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]


compile :: String -> Maybe Program
compile =
  parseExpr

compileAndRun :: String -> Either String StackVal
compileAndRun str =
  case parseExpr str of
    Just p -> stackVM p
    Nothing -> Left "Failed to parse program"


class HasVars a where
  var :: String -> a

data VarExprT
  = Var String
  | VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  deriving (Show, Eq)


instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add x y m = liftM2 (+) (x m) (y m)
  mul x y m = liftM2 (*) (x m) (y m)


withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
