{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

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

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                   deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  dice attackersUsed >>= \attackerRolls ->
  dice defendersUsed >>= \defenderRolls ->
    return $ computeBattle bf attackerRolls defenderRolls
  where
    attackersUsed = max 0 $ attackers bf - 1
    defendersUsed = min 2 $ defenders bf


computeBattle :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
computeBattle bf attackerRolls defenderRolls =
  result $ zip (highest attackerRolls) (highest defenderRolls)
  where
    highest = sortBy (flip compare)

    result :: [(DieValue, DieValue)] -> Battlefield
    result =
      uncurry Battlefield . foldr skirmish (attackers bf, defenders bf)

    skirmish (ar, dr) (as, ds) =
      if ar <= dr then
        (as - 1, ds)
      else
        (as, ds - 1)


invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf == 0 = return bf
  | otherwise = battle bf >>= invade


successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  successChance <$> replicateM trials (invade bf)
  where
    trials = 1000
    successChance = average . length . filter success
    success = (== 0) . defenders
    average count = fromIntegral count / fromIntegral trials
