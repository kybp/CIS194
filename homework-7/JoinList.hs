{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

tagSize :: (Sized m, Monoid m) => JoinList m a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l r)
  | i >= getSize (size m) = Nothing
  | i <  leftSize         = indexJ i l
  | otherwise             = indexJ (i - leftSize) r
  where leftSize = tagSize r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl           = jl
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append m l r)
  | n >= getSize (size m) = Empty
  | n <  leftSize         = dropJ n l +++ r
  | otherwise             = dropJ (n - leftSize) r
  where leftSize = tagSize r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _              = Empty
takeJ _ Empty          = Empty
takeJ _ x@(Single _ _) = x
takeJ n x@(Append m l r)
  | n >= getSize (size m) = x
  | n >= leftSize         = l +++ takeJ (n - leftSize) r
  | otherwise             = takeJ (n - leftSize) r
  where leftSize = tagSize r

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ x) = x
  toString (Append _ x y) = toString $ x +++ y
  fromString s = Single (scoreString s, Size 1) s
  line = indexJ
  replaceLine i s jl = before +++ (fromString s) +++ after
    where before = takeJ (i - 1) jl
          after  = dropJ i       jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main :: IO ()
main = runEditor editor $ fromStrings
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
  where fromStrings = (fromString . unlines
                       :: [String] -> JoinList (Score, Size) String)
