{-# OPTIONS_GHC -Wall #-}
module Prime where

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (2 :) $ map ((+ 1) . (* 2)) $ filter valid [1..n]
  where is_and_js = cartProd [1..n `div` 2] [n `div` 2..n]
        valid x   = not $ any (\(i, j) -> i + j + 2 * i * j <= x) is_and_js
