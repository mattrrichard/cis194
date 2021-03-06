{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) a Empty = a
(+++) Empty b = b
(+++) a b = Append (tag a <> tag b) a b


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


sizedTag :: (Sized m, Monoid m) => JoinList m a -> Int
sizedTag = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _  | i < 0 = Nothing
indexJ i jl | sizedTag jl < i = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l r)
  | lSize > i = indexJ i l
  | otherwise = indexJ (i - lSize) r
  where lSize = sizedTag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl
  | n <= 0 = jl
  | n >= sizedTag jl = Empty
dropJ n (Append _ l r)
  | n >= lSize = dropJ (n-lSize) r
  | otherwise = dropJ n l +++ r
  where lSize = sizedTag l
-- this next case is only possible if `Single` nodes can have size > 1,
-- and since no real way to split them, just kill the whole node
dropJ _ (Single _ _) = Empty


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl
  | n <= 0 = Empty
  | n >= sizedTag jl = jl
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ l r)
  | lSize < n = l +++ takeJ (n - lSize) r
  | otherwise = takeJ n l
  where lSize = sizedTag l


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- this probably sucks and definitely needs a refactor
balance :: (Sized b, Monoid b) => JoinList b a -> JoinList b a
balance Empty = Empty
balance jl@(Single _ _) = jl
balance jl@(Append m l r)
  | diff <= 1 = jl
  | sizeL < sizeR = recurse $ Append m (l +++ takeJ diff2 r) (dropJ diff2 r)
  | otherwise = recurse $ Append m (takeJ diffL l) (dropJ diffL l +++ r)
  where
    sizeL = sizedTag l
    sizeR = sizedTag r
    diff = abs (sizeL - sizeR)
    diff2 = diff `div` 2
    diffL = sizeL - diff2
    recurse (Append _ l' r') = Append m (balance l') (balance r')

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r

  fromString = balance . foldr ((+++) . single) Empty . lines
    where single s = Single (scoreString s, 1) s

  line = indexJ
  replaceLine n l b
    | n >= 0 && n < sizedTag b = takeJ n b +++ fromString  l +++ dropJ (n+1) b
    | otherwise = b

  numLines = sizedTag

  value Empty = 0
  value (Single (Score s, _) _) = s
  value (Append (Score s, _) _ _) = s


main :: IO ()
main =
  runEditor editor (Empty :: JoinList (Score, Size) String)
