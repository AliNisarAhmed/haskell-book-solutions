module Semigroup2 where

  import Test.QuickCheck

  data Two a b
    = Two a b deriving (Eq, Show)

  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ (Two a b)

  semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  type TwoString = Two String [String]

  type TwoStringAssoc = TwoString -> TwoString -> TwoString -> Bool

  main :: IO ()
  main = 
    quickCheck (semigroupAssoc :: TwoStringAssoc)