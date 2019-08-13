module FoldableInstances where

import Data.Monoid
import Data.Foldable

-- data Identity a
--   = Identity a deriving (Eq, Show)

-- instance Foldable Identity where
--   foldr f z (Identity x) = f x z
--   foldl f z (Identity x) = f z x
--   foldMap f (Identity x) = f x

-- data Optional a
--   = J a
--   | N deriving (Eq, Show)

-- instance Foldable Optional where
--   foldr f z (J x) = f x z
--   foldr f z N = z
--   foldl f z (J x) = f z x
--   foldl f z N = z
--   foldMap f (J x) = f x
--   foldMap f N = mempty

-----------------------  EXERCISES -----------------------

sum' ::(Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs
-- sum' xs = foldr (+) 0 xs

product' :: (Foldable t, Num a) => t a -> a
-- product' = foldr (*) 1
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . (== x)) xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a