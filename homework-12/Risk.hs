{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Risk where

import Control.Monad (replicateM)
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

throws :: Battlefield -> Rand StdGen ([DieValue],[DieValue])
throws (Battlefield attacking defending) =
  (,) <$> throw attacking' <*> throw defending'
  where attacking' = min (attacking - 1) 3
        defending' = min defending 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  throws bf >>= \(attacking, defending) ->
  let (wins, losses) = partition (uncurry (>)) $ zip attacking defending
      attacking'     = attackers bf - length losses
      defending'     = defenders bf - length wins
  in return $ Battlefield attacking' defending'

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield 1 _) = return b
invade b@(Battlefield _ 0) = return b
invade bf = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  replicateM 1000 (invade bf) >>= \results ->
  let wins  = fromIntegral . length $ filter ((== 0) . defenders) results
      total = fromIntegral $ length results in
  return $ wins / total
