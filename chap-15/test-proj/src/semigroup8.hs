module Semigroup8 where

  import Test.QuickCheck

  data Or a b 
    = Fst a
    | Snd b deriving (Eq, Show)

  instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Fst x <> Fst y = Fst y
    Snd x <> _ = Snd x
    Fst _ <> Snd x = Snd x

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      oneof [return $ Fst a, return $ Snd b]

  semiGroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiGroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  type OrStrings = Or String String
  type OrInts = Or Ordering Ordering

  type OrStringAssoc = OrStrings -> OrStrings -> OrStrings -> Bool
  type OrIntAssoc = OrInts -> OrInts -> OrInts -> Bool
  
  main :: IO ()
  main = do
    quickCheck (semiGroupAssoc :: OrIntAssoc)
    quickCheck (semiGroupAssoc :: OrStringAssoc)