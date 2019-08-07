module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

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
  arbitrary = frequency [(1, return Nope)]

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- main = do
--   quickBatch $ functor (Nope :: (Nope (Int, String, Int)))
--   quickBatch $ applicative (Nope :: (Nope (Int, String, Int)))
--   quickBatch $ monad (Nope :: (Nope (Int, String, Int)))

-------------------------------------------------------------------------

data BahEither b a
  = PLeft a
  | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap f (PRight b) = PRight (b)

instance Monoid b => Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight b1) (PRight b2) = PRight (b1 <> b2)
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b

instance Monoid b => Monad (BahEither b) where
  return = pure
  (>>=) (PLeft x) f = f x
  (>>=) (PRight x) _ = PRight x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ PLeft x), (1, return $ PRight y)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

type BahEitherTest = BahEither String (Int, String, Int)

-- main :: IO ()
-- main = do
--   quickBatch $ functor (PRight "Hello" :: BahEitherTest)
--   quickBatch $ applicative (PRight "Hello" :: BahEitherTest)
--   quickBatch $ monad (PRight "Hello" :: BahEitherTest)

-----------------------------------------------------------------------------

newtype Identity a
  = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

type IdentityTest = Identity (Int, String, Int)

-- main :: IO ()
-- main = do
--   quickBatch $ functor (Identity (1, "ali", 2) :: IdentityTest)
--   quickBatch $ applicative (Identity (1, "ali", 2) :: IdentityTest)
--   quickBatch $ monad (Identity (1, "ali", 2) :: IdentityTest)


-------------------------------------------------------------------------------

append :: List a -> List a -> List a
append Nil ys = ys
append ys Nil = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

data List a
  = Nil
  | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x list) = Cons (f x) (fmap f list)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) ys = fmap f ys `append` (fs <*> ys)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x `append` ((>>=) xs f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Cons x (Cons y Nil)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

type ListTest = List (Int, String, Int)

main :: IO ()
main = do
  quickBatch $ functor (Nil :: ListTest)
  quickBatch $ applicative (Nil :: ListTest)
  quickBatch $ monad (Nil :: ListTest)

---------------------------------------

j :: Monad m => m (m a) -> m a
-- j m = do
--   x <- m
--   x
-- OR
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = fmap f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf <*> m

-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh [] _ = return []
-- meh xs f = return (giveMeList xs f)
--   where
--     giveMeList :: Monad m => [a] -> (a -> m b) -> [b]
--     giveMeList [] _ = []
--     giveMeList list@(a:as) f = do
--       b <- f a
--       b: (giveMeList as f)

-- -- m :: (Monad m, Monad n) => [m a] -> [n a]
-- m xs = do
--   b <- xs
--   return [b]

-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh [] _ = return []
-- meh (x:xs) f = do
--   let list = (:) <$> (f x) <*> meh xs f  -- FUCKING FINALLY!!!!
--   list

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> (f x) <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = (flip meh) id