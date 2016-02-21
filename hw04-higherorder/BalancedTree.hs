module BalancedTree where

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h


insert :: a -> Tree a -> Tree a
insert a Leaf = Node 1 Leaf a Leaf
insert a (Node _ left e right) =
  let
    (left', right') =
      if (height left) <= (height right) then
        (insert a left, right)
      else
        (left, insert a right)

    newHeight = 1 + max (height left') (height right')
  in
    Node newHeight left' e right'


isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left _ right) =
  diff <= 1 && isBalanced left && isBalanced right
  where
    diff =
      abs $ (height left) - (height right)


foldTree :: [a] -> Tree a
foldTree =
  foldr insert Leaf


xor :: [Bool] -> Bool
xor =
  foldr (\a x -> if x then not a else a) False


cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys =
  [(x,y) | x <- xs, y <- ys ]




sundaramSieve :: Integer -> [Integer]
sundaramSieve n =
  map ((+1).(*2))
  $ foldr (filter . ( /=)) [1..n] ijs
  where
    ijs =
     map (\(i,j) -> i + j + 2*i*j)
      $ filter (\(i,j) -> i <= j && (i + j + 2*i*j)<= n)
      $ cartProd [1..n] [1..n]



