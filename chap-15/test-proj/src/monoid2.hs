module Monoid2 where

  import Test.QuickCheck

  data Two a b 
    = Two a b deriving (Eq, Show)

  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

  instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

  semiAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId m = mappend mempty m == m

  monoidRightId :: (Eq m, Monoid m) => m -> Bool
  monoidRightId m = mappend m mempty == m

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

  type TwoStr = Two String [String]
  type TwoStrings = TwoStr -> TwoStr -> TwoStr -> Bool 

  main :: IO ()
  main = do 
    quickCheck (semiAssoc :: TwoStrings)
    quickCheck (monoidLeftId :: TwoStr -> Bool)
    quickCheck (monoidRightId :: TwoStr -> Bool)
  