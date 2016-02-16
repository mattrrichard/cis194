{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

-- import ExprT
import Parser
import StackVM 

-- eval :: ExprT -> Integer
-- eval (Lit x) = x
-- eval (Add x y) = eval x + eval y
-- eval (Mul x y) = eval x * eval y


-- evalStr :: String -> Maybe Integer
-- evalStr =
--   fmap eval . parseExp Lit Add Mul


class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a


-- instance Expr ExprT where
--   lit = Lit
--   mul = Mul
--   add = Add


-- reify :: ExprT -> ExprT
-- reify = id


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
