module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

main = do
  quickBatch $ functor testVar
  quickBatch $ applicative testVar
  quickBatch $ monad testVar

