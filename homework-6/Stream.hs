{-# OPTIONS_GHC -Wall #-}
module Stream where

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream n s) = n : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat n = Stream n $ streamRepeat n

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream n s) = Stream (f n) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Stream n $ streamFromSeed f (f n)

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream n s) (Stream m t) =
  Stream n $ Stream m $ interleaveStreams s t

ruler :: Stream Integer
ruler = streamMap (f . succ) nats
  where f n | even n    = succ $ f $ n `div` 2
            | otherwise = 0
