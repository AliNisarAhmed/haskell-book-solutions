module Semigroup6 where

  import Test.QuickCheck

  newtype BoolConj
    = BoolConj Bool deriving (Eq, Show)

  instance Semigroup BoolConj where
    BoolConj False <> _ = BoolConj False
    _ <> BoolConj False = BoolConj False
    BoolConj True <> BoolConj True = BoolConj True
  
  semiGroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semiGroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  instance Arbitrary BoolConj where
    arbitrary = do
      a <- (arbitrary :: Gen Bool)
      return $ BoolConj a 

  main :: IO ()
  main = 
    quickCheck (semiGroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)