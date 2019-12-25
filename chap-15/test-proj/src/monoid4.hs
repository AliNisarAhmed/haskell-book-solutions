module Monoid4 where

  import Test.QuickCheck

  newtype Combine a b 
    = Combine { unCombine :: (a -> b) }

  instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

  instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty

  --

  newtype Comp a 
    = Comp (a -> a)

  instance (Semigroup a) => Semigroup (Comp a) where
    Comp x <> Comp y = Comp (x <> y)

  instance (Monoid a) => Monoid (Comp a) where
    mempty = Comp mempty
  

  --