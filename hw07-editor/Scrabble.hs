 {-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)


instance Monoid Score where
  mempty = 0
  mappend = (+)


score :: Char -> Score
score c
  | ielem "aeioulnrst" = 1
  | ielem "dg" = 2
  | ielem "bcmp" = 3
  | ielem "fhvwy" = 4
  | ielem "k" = 5
  | ielem "jx" = 8
  | ielem "qz" = 10
  | otherwise = 0
  where ielem cs = toLower c `elem` cs


scoreString :: String -> Score
scoreString =
  sum . map score
