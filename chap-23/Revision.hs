module Revision where 

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

-- newtype State s a 
--   = State { runState :: s -> (a, s) }

data Die 
  = One 
  | Two 
  | Three
  | Four 
  | Five 
  | Six 
  deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n = 
  case n of 
    1 -> One 
    2 -> Two
    3 -> Three 
    4 -> Four 
    5 -> Five 
    6 -> Six
    _ -> error $ "Not between 1 to 6"

rollDie :: State StdGen Die 
rollDie = state $ do 
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die 
rollDie' = 
  intToDie <$> state (randomR (1, 6))


-------------------------------------------------
rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes n = do 
  let g = mkStdGen n
      (d1, g1) = randomR (1, 6) g
      (d2, g2) = randomR (1, 6) g1
      (d3, _) = randomR (1, 6) g2
  (intToDie d1, intToDie d2, intToDie d3) 

---------------------------------------------------

newtype Moi s a = 
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where 
  fmap f (Moi g) = Moi $ \s -> (f (fst $ g s), s)

instance Applicative (Moi s) where 
  pure x = Moi $ \s -> (x, s)
  (<*>) (Moi f) (Moi x) =
    Moi $ \s -> ((fst $ f s) (fst $ x s), s)

instance Monad (Moi s) where 
  return = pure 
  -- f :: s -> (a, s)
  -- g :: (a -> Moi s b)
  -- -> Moi s b
  (Moi f) >>= g = 
    Moi $ \s -> (runMoi $ g (fst $ f s)) s

  ------------------------------------------------------

fizzBuzz :: Integer -> String 
fizzBuzz n 
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5 == 0 = "Buzz"
  | mod n 3 == 0 = "Fizz"
  | otherwise = show n 

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = 
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do 
  xs <- get 
  let result = fizzBuzz n 
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]

-------------------------------------------------------------

getM :: Moi s s
getM = Moi $ \s -> (s, s)

putM :: s -> Moi s ()
putM s = Moi (\s -> ((), s))

evalM :: Moi s a -> s -> a 
evalM (Moi sa) s = fst $ sa s

modifyM :: (s -> s) -> Moi s ()
modifyM f = Moi $ \s -> ((), f s)