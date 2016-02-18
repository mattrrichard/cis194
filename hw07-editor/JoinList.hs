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


(!!?) :: [a] -> Int -> Maybe a
[]     !!? _        = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
