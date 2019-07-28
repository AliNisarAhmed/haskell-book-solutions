module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- data Bull
--   = Fools
--   | Twoo deriving (Eq, Show)

-- instance Arbitrary Bull where
--   arbitrary =
--     frequency [ (1, return Fools)
--               , (1, return Twoo) ]

-- instance Semigroup Bull where
--   (<>) _ _ = Fools

-- instance Monoid Bull where
--   mempty = Fools

-- instance EqProp Bull where
--   (=-=) = eq

-- main :: IO ()
-- main = quickBatch (monoid Twoo)
-----------------------------------------------------------------------
-- newtype Ziplist a
--   = Ziplist [a] deriving (Eq, Show)

-- instance (Semigroup a, Monoid a) => Semigroup (Ziplist a) where
--   (<>) = liftA2 mappend
--   -- (<>) (Ziplist xs) (Ziplist ys) = Ziplist (xs <> ys)

-- instance Monoid a => Monoid (Ziplist a) where
--   mempty = Ziplist []

-- instance Arbitrary a => Arbitrary (Ziplist a) where
--   arbitrary = Ziplist <$> arbitrary

-- -- instance Arbitrary a => Arbitrary (Sum a) where
-- --   arbitrary = Sum <$> arbitrary

-- instance Eq a => EqProp (Ziplist a) where
--   (=-=) = eq
-------------------------------------------------------------------------

-- List Applicative Exercise

data List a
  = Nil
  | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ b Nil = b
foldList f b (Cons h t) = f h (foldList f b t)

concatList :: List (List a) -> List a
concatList = foldList append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f xs = concatList $ fmap f xs

instance Semigroup a => Semigroup (List a) where
  (<>) Nil x = x
  (<>) x Nil = x
  (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Monoid a => Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f (Nil) = Nil

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) ys = fmap f ys `append` (<*>) fs ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ (Cons x (Cons y Nil))

instance Eq a => EqProp (List a) where
  (=-=) = eq

f = Cons (+1) (Cons (*2) Nil)
g = Cons 1 (Cons 2 Nil)
h = Cons 3 (Cons 4 Nil)

-- main :: IO ()
-- main = quickBatch $ applicative (Cons ((1 :: Int), "a", (3 :: Int)) Nil)

-----------------------------------------------------------------------------------

newtype Ziplist a
  = Ziplist [a] deriving (Eq, Show)

instance Functor Ziplist where
  fmap f (Ziplist xs) = Ziplist $ fmap f xs

instance Applicative Ziplist where
  pure x = Ziplist $ repeat x
  (<*>) (Ziplist fs) (Ziplist xs) = Ziplist (zipWith ($) fs xs)

instance Arbitrary a => Arbitrary (Ziplist a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Ziplist [x, y]

instance Eq a => EqProp (Ziplist a) where
  xs =-= ys =
    xs' `eq` ys'
      where
        xs' =
          let
            (Ziplist l) = xs
          in
            take 3000 l
        ys' =
          let
            (Ziplist l) = ys
          in
            take 3000 l

x = Ziplist [(+1), (*2), (+3)]
y = Ziplist [1, 2, 3]

-- main :: IO ()
-- main = quickBatch $ applicative (Ziplist [((1:: Int), (1 :: Int), (1 :: Int))])

------------------------------------------------------------------------------------

data Validation e a
  = Failure' e
  | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' x) (Failure' y) = Failure' (x <> y)
  (<*>) (Failure' x) _ = (Failure' x)
  (<*>) _ (Failure' x) = (Failure' x)
  (<*>) (Success' f) (Success' b) = Success' (f b)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure' e), (1, return $ Success' b)]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

type ValidationType = Validation [String] ([Int], [Int], [Int])

-- main :: IO ()
-- main = quickBatch $ applicative (Failure' ["hello"] :: ValidationType)

--------------------------------------------------------------------------------

data Pair a =
  Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

type PairApp = Pair (Int, String, Char)

-- main :: IO ()
-- main = quickBatch $ applicative (Pair (2, "a", 'b') (3, "c", 'd') :: PairApp)

--------------------------------------------------------------------------------

data Two a b
  = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a1 f) (Two a2 b) = Two (a1 <> a2) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- main :: IO ()
-- main = quickBatch $ applicative $ (Two "abc" (1, 2, 3) :: Two String (Int, Int, Int))

----------------------------------------------------------------------------------

-- data Three a b c
--   = Three a b c deriving (Eq, Show)

-- instance Functor (Three a b) where
--   fmap f (Three a b c) = Three a b (f c)

-- instance (Monoid a, Monoid b) => Applicative (Three a b) where
--   pure = Three mempty mempty
--   (<*>) (Three a1 b1 f) (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

-- instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     z <- arbitrary
--     return $ Three x y z

-- instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
--   (=-=) = eq

-- main :: IO ()
-- main = quickBatch $ applicative (Three "a" "b" (1, 2, 3) :: Three String String (Int, Int, Int))

---------------------------------------------------------------------------------------------

data Three a b
  = Three a b b deriving (Eq, Show)

instance Functor (Three a) where
  fmap f (Three a b1 b2) = Three a (f b1) (f b2)

instance Monoid a => Applicative (Three a) where
  pure x = Three mempty x x
  (<*>) (Three a1 f1 f2) (Three a2 x1 x2) = Three (a1 <> a2) (f1 x1) (f2 x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary -- `suchThat` (/= y) )
    return $ Three x y z

instance (Eq a, Eq b) => EqProp (Three a b) where
  (=-=) = eq

-- main :: IO ()
-- main =
--   quickBatch $ applicative ( (Three "a" (1, 2, 3) (4, 5, 6)) :: Three String (Int, Int, Int))

---------------------------------------------------------------------------------------------------

-- data Four a b c d
--   = Four a b c d deriving (Eq, Show)

-- instance Functor (Four a b c) where
--   fmap f (Four a b c d) = Four a b c (f d)

-- instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
--   pure = Four mempty mempty mempty
--   (<*>) (Four a1 b1 c1 f) (Four a2 b2 c2 x)
--     = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f x)

-- instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
--   Arbitrary (Four a b c d) where
--     arbitrary = do
--       a <- arbitrary
--       b <- arbitrary
--       c <- arbitrary
--       d <- arbitrary
--       return $ Four a b c d

-- instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
--   (=-=) = eq

-- type FourApp = Four String String String (Int, Int, Int)

-- main :: IO ()
-- main = quickBatch $ applicative (Four "a" "b" "c" (1, 2, 3) :: FourApp)

-------------------------------------------------------------------------------

data Four a b
  = Four a a a b deriving (Eq, Show)

instance Functor (Four a) where
  fmap f (Four a1 a2 a3 b) = Four a1 a2 a3 (f b)

instance Monoid a => Applicative (Four a) where
  pure = Four mempty mempty mempty
  (<*>) (Four a1 a2 a3 f) (Four b1 b2 b3 x)
    = Four (a1 <> b1) (a2 <> b2) (a3 <> b3) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four a1 a2 a3 b

instance (Eq a, Eq b) => EqProp (Four a b) where
  (=-=) = eq

type FourApp = Four String (Int, Int, Int)

main :: IO ()
main =
  quickBatch $ applicative (Four "a" "b" "c" (1, 2, 3) :: FourApp)

--------------------------------------------------------------------------------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))