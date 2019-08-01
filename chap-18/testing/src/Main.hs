module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join, (>=>))

data Countme a
  = Countme Integer a deriving (Eq, Show)

instance Functor Countme where
  fmap f (Countme n x) = Countme n (f x)

instance Applicative Countme where
  pure = Countme 0
  (<*>) (Countme n f) (Countme m x) = Countme (n + m) (f x)

instance Monad Countme where
  return = pure
  (>>=) (Countme n x) f =
    Countme (n + m) b
      where Countme m b = f x

instance Arbitrary a => Arbitrary (Countme a) where
  arbitrary = Countme <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Countme a) where
  (=-=) = eq

type CountTest = Countme (Int, String, Int)

testVar :: CountTest
testVar = Countme 2 (2, "abc", 3)

-- main = do
--   quickBatch $ functor testVar
--   quickBatch $ applicative testVar
--   quickBatch $ monad testVar

-----------------------------------------------------

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- mcomp f g a = join $ f <$> g a
mcomp f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! how old are you?"

-------------------------------------------------------

-- Monad instances --

-- 1.

data Nope a
  = Nope deriving (Eq, Show)

instance Functor Nope where
  fmap _ Nope = Nope

instance Applicative Nope where
  pure x = Nope
  (<*>) _ _ = Nope

instance Monad Nope where
  return x = Nope
  (>>=) Nope f = Nope

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = do
    x <- arbitrary
    return Nope

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

main = do
  quickBatch $ functor (Nope :: (Nope (Int, String, Int)))
  quickBatch $ applicative (Nope :: (Nope (Int, String, Int)))
  quickBatch $ monoid (Nope :: (Nope (Int, String, Int)))
