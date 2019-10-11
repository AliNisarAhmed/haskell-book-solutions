module RandomExample2 where

import Control.Applicative(liftA3)
import Control.Monad(replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample

rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum c@(count, listOfDie) gen
      | sum >= n = c
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1, (intToDie die): listOfDie) gen