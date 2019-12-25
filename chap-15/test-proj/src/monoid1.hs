module Monoid1 where

  import Test.QuickCheck

  newtype Identity a
    = Identity a deriving (Eq, Show)


  instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

  instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a
  
  semiAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId m = mappend mempty m == m

  monoidRightId :: (Eq m, Monoid m) => m -> Bool
  monoidRightId m = mappend m mempty == m

  type IdStr = Identity String

  type IdentStrings = IdStr -> IdStr -> IdStr -> Bool

  main :: IO ()
  main = do
    quickCheck (semiAssoc :: IdentStrings)
    quickCheck (monoidLeftId :: IdStr -> Bool)
    quickCheck (monoidRightId :: IdStr -> Bool)