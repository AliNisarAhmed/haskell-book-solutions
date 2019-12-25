module SMGRP where
  
  import Test.QuickCheck

  data Trivial 
    = Trivial deriving (Eq, Show)

  instance Semigroup Trivial where
    _ <> _ = Trivial

  instance Arbitrary Trivial where
    arbitrary = return Trivial

  -- semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  -- semigroupAssoc a b c = 
  --   (a <> (b <> c)) == ((a <> b) <> c)

  type TrivAssoc = 
    Trivial -> Trivial -> Trivial -> Bool

  -- main :: IO ()
  -- main = 
  --   quickCheck (semigroupAssoc :: TrivAssoc)

  -- ** 2 ** 

  newtype Identity a = Identity a deriving (Eq, Show)

  instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)
      
  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a

  semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  type IdentityStr = Identity Integer

  type IdentityAssoc
    = IdentityStr -> IdentityStr -> IdentityStr -> Bool 
  
  main :: IO ()
  main = 
    quickCheck (semigroupAssoc :: IdentityAssoc)
