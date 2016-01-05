{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Data.List (sort)
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f1}) (GL es f2) =
  GL (e:es) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) =
    GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f x (Node {rootLabel = y, subForest = children}) =
  f y (map (treeFold f x) children)

nextList :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextList boss bestLists = (bestWithout, bestWith)
  where maximum' [] = mempty
        maximum' l  = maximum l
        bestWithout = maximum' $ map fst bestLists
        bestWith    = maximum' $ map (glCons boss . snd) bestLists

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextList (mempty, mempty)

formatReport :: GuestList -> String
formatReport (GL employees fun) = unlines $ funReport : sortedEmployees
  where funReport       = "Total fun: " ++ show fun
        sortedEmployees = sort $ map empName employees

main :: IO ()
main = readFile "company.txt" >>= putStr . formatReport . maxFun . read
