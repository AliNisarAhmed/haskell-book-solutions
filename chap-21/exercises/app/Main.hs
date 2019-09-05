module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad
import Data.Monoid

newtype Identity a
  = Identity a deriving (Eq, Show, Ord)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f x = sequenceA $ fmap f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

type IdentityTest = Identity (Sum Int, String, Maybe Int)

testVar :: IdentityTest
testVar = Identity (Sum 2, "abc", Just 300)

type TI = []

trigger :: TI (Maybe Int, String, [Int])
trigger = [(Just 2, "abc", [1, 2, 3])]

main :: IO ()
main = do
  quickBatch $ functor testVar
  quickBatch $ applicative testVar
  quickBatch $ monad testVar
  quickBatch $ traversable testVar
