module StateWikiBook where 

import Control.Applicative
import Control.Monad.Trans.State
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = traverse id $ replicate n (randomRIO (1, 6))

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g1 = 
  let 
    (n, g2) = randomR (1, 6) g1
    (m, g3) = randomR (1, 6) g2
  in 
    ((n, m), g3)

newtype State s a
  = State { runState :: s -> (a, s) }

instance Functor (State s) where 
  fmap f (State sa) = 
    State $ \s -> 
      (f $ fst $ sa s, s)

instance Applicative (State s) where 
  pure x = State $ \s -> (x, s)
  -- fs :: s -> (f, s)
  -- as :: s -> (a, s)
  (<*>) (State fs) (State as) = 
    State $ \s -> 
      ((fst $ fs s) (fst $ as s), s)

instance Monad (State s) where 
  return = pure 
  -- f :: a -> State s b :: a -> s -> (b, s)
  -- State s b :: s -> (b, s)
  as >>= f = 
    State $ \s -> 
      let 
        (x, s1) = runState as s
      in 
        runState (f x) s1

-----------------------------------------

randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
