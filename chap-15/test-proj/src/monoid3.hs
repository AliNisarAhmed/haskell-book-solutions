module Monoid3 where

  import Test.QuickCheck

  newtype BoolConj
    = BoolConj Bool deriving (Eq, Show)

  instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    _ <> BoolConj False = BoolConj False
    BoolConj True <> BoolConj True = BoolConj True

  instance Monoid BoolConj where
    mempty = BoolConj True

  semiAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId m = mappend mempty m == m

  monoidRightId :: (Eq m, Monoid m) => m -> Bool
  monoidRightId m = mappend m mempty == m
  
  instance Arbitrary BoolConj where
    arbitrary = do
      a <- elements [True, False]
      return $ BoolConj a
      
  type BCB = BoolConj
  type BCB3 = BCB -> BCB -> BCB -> Bool

  main :: IO ()
  main = do 
    quickCheck (semiAssoc :: BCB3)
    quickCheck (monoidLeftId :: BCB -> Bool)
    quickCheck (monoidRightId :: BCB -> Bool)