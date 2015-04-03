{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sort)

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
    deriving Show

-- Exercise #1:

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do 
    atkDice <- (roll attackForce)
    defDice <- (roll defenseForce)
    let lineUp = zip (sort atkDice) (sort defDice)
        winners = map (uncurry (>)) lineUp
        defenseLost = length $ filter id  winners
        offenseLost = length $ filter not winners
    return (Battlefield (atk - offenseLost) (def - defenseLost))
    where 
      atk = attackers bf
      def = defenders bf
      attackForce  = if atk > 4 then 3 else min (atk - 1) 3
      defenseForce = if def > 2 then 2 else def


roll :: Int -> Rand StdGen [DieValue]
roll n = replicateM n die

-- Exercise #2:

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    newBf <- battle bf
    if attackers newBf > 2 && defenders newBf > 0  
       then invade newBf
       else return newBf
