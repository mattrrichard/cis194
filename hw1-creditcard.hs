module CreditCard where

import Control.Arrow

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


toDigits :: Integer -> [Integer]
toDigits n =
  reverse $ toDigitsRev n


-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther digits =
--   map (\(i, d) -> if i `mod` 2 == 0 then d * 2 else d)
--    (zip [1..] digits)

doubleEvens :: [Integer] -> [Integer]
doubleEvens =
  zip [1..]
    >>> map (\(i, d) -> if i `mod` 2 == 0 then d * 2 else d)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  reverse >>> doubleEvens >>> reverse


-- sumOfDigits :: [Integer] -> Integer
-- sumOfDigits [] = 0
-- sumOfDigits (x:xs) = (toDigits x + sumOfDigits xs

sumOfDigits :: [Integer] -> Integer
sumOfDigits digits =
  let
    expanded = map toDigits digits
  in
    sum (map sum expanded)

-- validate :: Integer -> Bool
validate :: Integer -> Bool
validate =
  toDigitsRev
    >>> doubleEvens
    >>> sumOfDigits
    >>> (\n -> n `mod` 10 == 0)
