 {-# LANGUAGE FlexibleInstances #-}
 {-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Streams where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n >= 2 = fib (n-1) + fib (n-2)
  | n < 0  = fib (n+2) - fib (n+1)
fib _ = 0 --included only to make the compiler stop yelling at me.
-- I don't know why you think this isn't an exhaustive match without the catch all,
-- but I'm pretty certain you're wrong


fib1 :: [Integer]
fib1 = map fib [0..]


fib2 :: [Integer]
fib2 =
  zipWith (+) (0 : 1 : fib2) (0 : fib2)


fib3 :: [Integer]
fib3 =
  fib' 0 1
  where
    fib' a b = a : fib' b (a+b)


data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as


streamRepeat :: a -> Stream a
streamRepeat a =
  Cons a (streamRepeat a)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) =
  Cons (f a) (streamMap f as)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =
  Cons a (streamFromSeed f (f a))


nats :: Stream Integer
nats =
  streamFromSeed succ 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) b =
  Cons a (interleaveStreams b as)

ruler :: Stream Integer
ruler =
  let
   ruler' n =
      interleaveStreams (streamRepeat n) (ruler' $ n+1)
  in
    ruler' 0


x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)

  negate = streamMap (*(-1))

  (+) (Cons a as) (Cons b bs) =
    Cons (a+b) (as + bs)

  (*) (Cons a0 a') b@(Cons b0 b') =
    Cons (a0 * b0) (streamMap (*a0) b' + (a' * b))


instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') =
    q
    where
      q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))


fibsStream :: Stream Integer
fibsStream =
  x / (1 - x - x*x)


data Matrix =
  Matrix (Integer, Integer, Integer, Integer)
  deriving Show


instance Num Matrix where
  (*) (Matrix (a11, a12, a21, a22)) (Matrix (b11, b12, b21, b22)) =
    Matrix ( a11 * b11 + a12 * b21, a11 * b12 + a12 * b22
           , a21 * b11 + a22 * b21, a21 * b12 + a22 * b22 )


fibN :: Integer -> Integer
fibN 0 = 0
fibN 1 = 1
fibN n =
  a11 (f^(n-1))
  where
    f = Matrix (1,1,1,0)
    a11 (Matrix (a, _, _, _)) = a
