module CodeGolf where

skip :: [a] -> Int -> [a]
skip (x:xs) n =
  x : skip (drop n (x:xs)) n

skip [] _ = []


skips :: [a] -> [[a]]
skips a =
   map (skip a) [1..length a]


localMaximum :: [Integer] -> [Integer]
localMaximum l =
  map (\(_,m,_) -> m)
  $ filter (\(a, b, c) -> b > a && b > c)
  $ zip3 l (drop 1 l) (drop 2 l)


histogram :: [Integer] -> String
histogram l =
  let
    counts = map (count l) [0..9]
    maxC = foldr max 0 counts
    histrow r =
      map (\n -> if n >= r then '*' else ' ') counts ++ "\n"
  in
    concatMap histrow [maxC, maxC - 1..1]
    ++ "==========\n0123456789\n"

count :: [Integer] -> Integer -> Int
count l n =
  length . filter (== n) $ l

hist2 l =
  unlines (map line [m+1,m..1]) ++ "==========\n0123456789\n"
  where
    c = map (count l) [0..9]
    m = foldr max 0 c
    line n =
      [if i >= n then '*' else ' ' | i <- c]
