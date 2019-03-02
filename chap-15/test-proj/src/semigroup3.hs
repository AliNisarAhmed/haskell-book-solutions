module Semigroup3 where

  import Test.QuickCheck

  data Three a b c 
    = Three a b c deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)
    
  semiGroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiGroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

  type ThreeStrings = Three String [String] String

  type ThreeStringAssoc = ThreeStrings -> ThreeStrings -> ThreeStrings -> Bool

  main :: IO ()
  main = 
    quickCheck (semiGroupAssoc :: ThreeStringAssoc