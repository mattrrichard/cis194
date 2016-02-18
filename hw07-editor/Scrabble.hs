 {-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)


instance Monoid Score where
  mempty = 0
  mappend = (+)


score :: Char -> Score
score c
  | c `elem` "aeioulnrst" = 1
  | c `elem` "dg" = 2
  | c `elem` "bcmp" = 3
  | c `elem` "fhvwy" = 4
  | c == 'k' = 5
  | c `elem` "jx" = 8
  | c `elem` "qz" = 10
  | otherwise = 0


scoreString :: String -> Score
scoreString =
  sum . map score
