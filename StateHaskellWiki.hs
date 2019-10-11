-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State#cite_ref-3

module StateHaskellWiki where

import Control.Monad.Trans.State
import System.Random

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = traverse (\_ -> randomRIO (1, 6)) [1..n]

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
  where
    (n, g) = randomR (1, 6) (mkStdGen 0)
    (m, _) = randomR (1, 6) g

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((n, m), newGen)
    where
      (n, g1) = randomR (1, 6) g
      (m, newGen) = randomR (1, 6) g1

compose :: (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s)
compose f g = \s ->
  let
    (a1, s1) = f s
  in
    g a1 s

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)