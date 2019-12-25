module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5  == 0 = "Buzz"
  | n `mod` 3  == 0 = "Fizz"
  | otherwise       = show n


fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  let
    dlist = execState (mapM_ addResult list) DL.empty
  in
    DL.apply dlist []

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList [1..100]


-- WIthout DLIST

-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list =
--   -- execState :: State s a -> s -> s  => "fills up" the state
--   execState (mapM_ addResult list) []

-- addResult :: Integer -> State [String] ()
-- addResult n = do
--   xs <- get
--   let result = fizzBuzz n
--   put (result : xs)

-- main :: IO ()
-- main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]