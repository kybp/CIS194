{-# OPTIONS_GHC -Wall #-}
module FoldTree where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr foldTree' Leaf
  where
    -- Inserting into an empty tree results in a height of 0
    foldTree' x Leaf = Node 0 Leaf x Leaf
    -- Otherwise, our height is one more than that of our child
    foldTree' x (Node h l y r)
      -- If the left tree is taller, insert on the right
      | (height l) > (height r) = Node h' l y $ foldTree' x r
      | otherwise               = Node h (foldTree' x l) y r
      where h' = h + 1
            height (Node n _ _ _) = n
            height Leaf = -1
