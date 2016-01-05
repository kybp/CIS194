{-# OPTIONS_GHC -Wall #-}
module Folds where

xor :: [Bool] -> Bool
xor = foldr (\x result -> if x then result else not result) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
