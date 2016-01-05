{-# OPTIONS_GHC -Wall #-}
module FoldTree where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    -- Inserting into an empty tree results in a height of 0
    insert x Leaf = Node 0 Leaf x Leaf
    -- Otherwise, our height is one more than that of our child
    insert x (Node _ l y r)
      -- If the left tree is taller, insert on the right
      | (height l) > (height r) = Node (succ (height newRight)) l y newRight
      | otherwise               = Node (succ (height newLeft)) newLeft y r
      where height (Node n _ _ _) = n
            height Leaf = -1
            newLeft  = insert x l
            newRight = insert x r
