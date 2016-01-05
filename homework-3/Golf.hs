{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips xs = map (flip every xs) [1..length xs]
  where every n ys = case drop (n - 1) ys of
          []     -> []
          (z:zs) -> z : every n zs

localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_,x,_) -> x) .
              filter (\(x,y,z) -> y > x && y > z) .
              triples
  where triples (x:t@(y:z:_)) = (x,y,z):(triples t)
        triples _ = []

makeRows :: [Integer] -> [String]
makeRows counts = if all (<= 0) counts
                  then []
                  else "\n" : thisRow : (makeRows newCounts)
  where thisRow   = map (\n -> if n <= 0 then ' ' else '*') counts
        newCounts = map (+ (-1)) counts

histogram :: [Integer] -> String
histogram ns = foldl (flip (++)) start $ makeRows $ getCounts ns
  where start = "==========\n0123456789\n"
        countN n = fromIntegral . length . filter (== n)
        getCounts xs = map (flip countN xs) [0..9]
        

