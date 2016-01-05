{-# OPTIONS_GHC -Wall #-}
module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (pred n) a c b) ++
                (hanoi 1 a b c) ++
                (hanoi (pred n) c b a)
