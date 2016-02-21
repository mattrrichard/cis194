{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.Monoid


glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) =
  GL (e : es) (f + empFun e)


instance Monoid GuestList where
  mempty = GL [] 0

  mappend a (GL es _) = foldr glCons a es


moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t =
  f (rootLabel t) (map (treeFold f) $ subForest t)


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls =
  (glCons e withoutSubBosses, withSubBosses)
  where
     (withSubBosses, withoutSubBosses) =
       {- combine subtree results:
          The withoutSubBosses result can _only_ be the second result in each
            because it must guarantee that none of the current node's direct
            subbordinates are in the list
          The withSubBosses result, however, should be the best result even if
            that means not including the root of that subtree.  See the subtree
            under 'Bob' in testCompany for an example of why this is important.
       -}
       foldr (\(w, wo) -> (<>) (moreFun w wo, wo)) mempty gls


maxFun :: Tree Employee -> GuestList
maxFun =
  uncurry moreFun . treeFold nextLevel


main :: IO ()
main =
  readFile "company.txt" >>= putStr . guestListToString . maxFun . read


guestListToString :: GuestList -> String
guestListToString (GL gl fun) =
  "Total fun: " ++ show fun ++ "\n" ++
     unlines (map empName gl)
