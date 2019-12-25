module Semigroup7 where

  import Test.QuickCheck

  -- Bool Disjunction

  newtype BoolD = BoolD Bool deriving (Eq, Show)

  instance Semigroup BoolD where
    BoolD True <> _ = BoolD True
    _ <> BoolD True = BoolD True
    BoolD False <> BoolD False = BoolD False

  
  instance Arbitrary BoolD where
    arbitrary = do
      a <- arbitrary :: Gen Bool
      return $ BoolD a

  semiGroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiGroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  main :: IO ()
  main = 
    quickCheck (semiGroupAssoc :: BoolD -> BoolD -> BoolD -> Bool)