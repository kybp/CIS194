{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Risk where

import Control.Monad.Random
import Data.List (partition, sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

throw :: Int -> Rand StdGen [DieValue]
throw n = throw' n >>= return . ascending
  where ascending = sortBy $ flip compare
        throw' 0  = pure []
        throw' i  = (:) <$> die <*> throw' (i - 1)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  throws bf >>= \(attacking, defending) ->
  let contests       = zip (take (length defending) attacking) defending
      (wins, losses) = partition (uncurry (>)) contests
      attacking'     = attackers bf - length losses
      defending'     = defenders bf - length wins
  in return $ Battlefield attacking' defending'
  where throws (Battlefield attacking defending) =
          (,) <$> throw (min 3 attacking) <*> throw (min 2 defending)
