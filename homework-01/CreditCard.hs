{-# OPTIONS_GHC -Wall #-}
module CreditCard where

toDigits :: Integer -> [Integer]
toDigits x =
  let digits' acc n =
        if n < 10
        then n : acc
        else digits' (n `mod` 10 : acc) (n `div` 10)
  in if x < 1
     then []
     else digits' [] x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . aux . reverse
  where aux [] = []
        aux [x] = [x]
        aux (x:x':xs) = x : (x' * 2) : (aux xs)

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) .  sum . toDigits) 0

validate :: Integer -> Bool
validate = (== 0) . (flip rem 10) . sumDigits . doubleEveryOther . toDigits
