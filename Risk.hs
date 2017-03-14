{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Loops
import           Control.Monad.Random
import           Data.List

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

data Side = Attacking | Defending deriving Eq

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = do
  attackerDice <- armyDice participatingAttackers
  defenderDice <- armyDice participatingDefenders
  let fightResults = fight attackerDice defenderDice in
    return Battlefield {
    attackers=remainingAttackers + (participatingAttackers - (casualties Attacking fightResults)),
    defenders=remainingDefenders + (participatingDefenders - (casualties Defending fightResults))
    }
  where (participatingAttackers, remainingAttackers) = splitArmy attackers Attacking
        (participatingDefenders, remainingDefenders) = splitArmy defenders Defending

invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM doesBattleContinue battle

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
  battles <- replicateM 1000 (battle battlefield)
  return $ fromIntegral (successes battles) / 1000.0

successes :: [Battlefield] -> Int
successes = length . filter (==Attacking) . map winner

armyDice :: Army -> Rand StdGen [DieValue]
armyDice = (`replicateM` getRandom)

splitArmy :: Army -> Side -> (Army, Army)
splitArmy army Attacking
  | (army >= 4) = (3, army - 3)
  | otherwise = (army - 1, 1)
splitArmy army Defending
  | (army >= 2) = (2, army - 2)
  | otherwise = (army, 0)

fight :: [DieValue] -> [DieValue] -> [Ordering]
fight attackers defenders = zipWith compare (reverse . sort $ attackers) (reverse . sort $ defenders)

casualties :: Side -> [Ordering] -> Army
casualties Attacking = length . filter (/=LT)
casualties Defending = length . filter (==GT)

doesBattleContinue :: Battlefield -> Bool
doesBattleContinue (Battlefield attackers defenders) = (attackers >= 2) && (defenders >= 1)

winner :: Battlefield -> Side
winner (Battlefield attackers defenders)
  | defenders == 0 = Attacking
  | otherwise = Defending
