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
  foldr f z (Identity a) = f a z

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
  -- foldMap f (Constant a) = mempty
  foldr f mempty (Constant a) = mempty

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
  -- foldMap _ Nada = mempty
  -- foldMap f (Yep x) = f x
  foldr _ z (Nada) = z
  foldr f z (Yep x) = f x z

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
  -- foldMap f Nil = mempty
  -- foldMap f (Cons x xs) = f x <> foldMap f xs
  foldr _ z Nil = z
  foldr f z (Cons a list) = f a (foldr f z list)

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

-- main :: IO ()
-- main = do
--   quickBatch $ monoid testList
--   quickBatch $ functor testList
--   quickBatch $ applicative testList
--   quickBatch $ monad testList
--   quickBatch $ traversable testConstant

-------------------------------------------------

data Three a b c
  = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
    (<>) (Three a1 b1 c1) (Three a2 b2 c2) =
      Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a, Monoid b, Monoid c) =>
  Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a1 b1 f) (Three a2 b2 x) =
    Three (a1 <> a2) (b1 <> b2) (f x)

instance (Monoid a, Monoid b) => Monad (Three a b) where
  return = pure
  (Three a b x) >>= f = Three (a <> a1) (b <> b1) c1
    where
      Three a1 b1 c1 = f x

instance Foldable (Three a b) where
  -- foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type ThreeType
  = Three String (Sum Int) (Int, String, [Maybe Int])

threeVar :: ThreeType
threeVar = Three "abc" (Sum 2) (2, "xyz", [Just 1, Just 2])

-- main :: IO ()
-- main = do
--   quickBatch $ functor threeVar
--   quickBatch $ applicative threeVar
--   quickBatch $ monad threeVar
--   quickBatch $ traversable testConstant

-------------------------------------------------------------

data Pair a b
  = Pair a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) (Pair a1 b1) (Pair a2 b2) = Pair (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  (<*>) (Pair a1 f) (Pair a2 x) = Pair (a1 <> a2) (f x)

instance Monoid a => Monad (Pair a) where
  return = pure
  (Pair a1 b1) >>= f = Pair (a1 <> a2) b2
    where Pair a2 b2 = f b1

instance Foldable (Pair a) where
  -- foldMap f (Pair a b) = f b
  foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

type PairType = Pair (Sum Int) (Int, String, [Int])

pairVar :: PairType
pairVar = Pair (Sum 2) (2, "abc", [1, 2, 3])

-- main :: IO ()
-- main = do
--   quickBatch $ functor pairVar
--   quickBatch $ applicative pairVar
--   quickBatch $ monad pairVar
--   quickBatch $ traversable pairVar

-----------------------------------------------------------

data Big a b
  = Big a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (Big a b) where
    (<>) (Big a1 b1 b2) (Big a2 b3 b4)
      = Big (a1 <> a2) (b1 <> b3) (b2 <> b4)

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance (Monoid a) => Applicative (Big a) where
  pure x = Big mempty x x
  (<*>) (Big a1 f1 f2) (Big a2 x1 x2) =
    Big (a1 <> a2) (f1 x1) (f2 x2)

instance (Monoid a) => Monad (Big a) where
  return = pure
  (Big a1 b1 b2) >>= f = Big (a1 <> a2) b3 b4
    where
      Big a2 b3 b4 = f b1

instance Foldable (Big a) where
  -- foldMap f (Big a b1 b2) = f b1 <> f b2
  foldr f z (Big a b1 b2) = f (b1 <> b2) z

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> (f b1) <*> (f b2)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

type BigType = Big (Sum Int) (Int, String, [Int])

bigVar :: BigType
bigVar = Big (Sum 2) (2, "abc", [1, 2, 3]) (3, "xyz", [4, 5, 6])

main :: IO ()
main = do
  quickBatch $ functor bigVar
  quickBatch $ applicative bigVar
  quickBatch $ monad bigVar
  quickBatch $ traversable bigVar
