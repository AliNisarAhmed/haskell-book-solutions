module RandomExample where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad.Trans.State
import Control.Monad (replicateM)

data Die
  = DieOne
  | DieTWo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTWo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "must be between 1 and 6, got: " ++ show x

rollDieThreeTimes' :: (Die, Die, Die)
rollDieThreeTimes' = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)