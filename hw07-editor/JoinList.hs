module JoinList where

import Data.Monoid
-- import Buffer
import Sized
-- import Editor

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

