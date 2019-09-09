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
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- type IdentityTest = Identity (Sum Int, String, Maybe Int)

-- testVar :: IdentityTest
-- testVar = Identity (Sum 2, "abc", Just 300)

-- type TI = Identity

-- trigger :: TI ((Maybe Int, String, [Int]))
-- trigger = Identity (Just 2, "abc", [1, 2, 3])

-- main :: IO ()
-- main = do
--   quickBatch $ functor testVar
--   quickBatch $ applicative testVar
--   quickBatch $ monad testVar
--   quickBatch $ traversable trigger

-------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Constant a b) where
  (<>) (Constant x) (Constant y) = Constant (x <> y)

instance Monoid a => Monoid (Constant a b) where
  mempty = Constant mempty

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)

-- Constant does not have a Monad instance
-- see: https://stackoverflow.com/questions/11530412/monad-for-const
-- due to failing left ID law
instance Monoid a => Monad (Constant a) where
  return = pure
  (>>=) (Constant a) f = Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

type ConstantTest = Constant (Sum Int, String, Maybe (Sum Int)) ([Int], String, [Int])

testConstant :: ConstantTest
testConstant = Constant (Sum 2, "abc", Just 300)

-- main :: IO ()
-- main = do
--   quickBatch $ functor testConstant
--   quickBatch $ applicative testConstant
--   quickBatch $ monad testConstant
--   quickBatch $ traversable testConstant


------------------------------------------------------------


data Optional a
  = Nada
  | Yep a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada _ = Nada
  (<>) _ Nada = Nada
  (<>) (Yep x) (Yep y) = Yep (x <> y)

instance Semigroup a => Monoid (Optional a) where
  mempty = Nada

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure = Yep
  (<*>) Nada _ = Nada
  (<*>) _ Nada = Nada
  (<*>) (Yep f) (Yep x) = Yep (f x)

instance Monad Optional where
  return = Yep
  (>>=) (Yep x) f = f x
  (>>=) _ _ = Nada

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = Yep <$> arbitrary

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

type TestOpt = Optional (Sum Int, String, Maybe Int)

testOpt :: TestOpt
testOpt = Nada

-- main :: IO ()
-- main = do
--   quickBatch $ functor testOpt
--   quickBatch $ applicative testOpt
--   quickBatch $ monad testOpt
--   quickBatch $ traversable testConstant

------------------------------------------------------------

data List a
  = Nil
  | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) Nil Nil = Nil
  (<>) Nil x = x
  (<>) x Nil = x
  (<>) (Cons a listA) y = Cons a (listA <> y)

instance Semigroup a => Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x listA) = Cons (f x) (fmap f listA)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) ys = (fmap f ys) <> (fs <*> ys)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (>>=) (Cons x listX) f = (f x) <> (listX >>= f)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Cons x (Cons y Nil)

instance Eq a => EqProp (List a) where
  (=-=) = eq

type TestList = List (String, Sum Int, Any)

testList :: TestList
testList = Cons ("abc", 2, Any True) Nil

main :: IO ()
main = do
  quickBatch $ monoid testList
  quickBatch $ functor testList
  quickBatch $ applicative testList
  quickBatch $ monad testList
  quickBatch $ traversable testConstant