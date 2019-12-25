module MaybeMonoid where

  import Data.Monoid
  import Test.QuickCheck

  data Optional a
    = Nada
    | Only a deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
  
  instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only x) = Only x
    (Only x) <> Nada = Only x
    (Only x) <> (Only y) = Only $ (x <> y)
    Nada <> Nada = Nada

  newtype First' a = 
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

  instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
      a <- arbitrary
      frequency [(3, return $ First' (Only a)), (1, return $ First' Nada)]
  
  instance Monoid a => Monoid (First' a) where
    mempty = First' Nada
  
  instance Semigroup a => Semigroup (First' a) where
    First' (Only x) <> First' Nada = First' (Only x)
    First' Nada <> First' (Only x) = First' (Only x)
    First' (Only x) <> First' (Only y) = First' $ (Only x <> Only y)
    First' Nada <> First' Nada = First' Nada 

  monoidAssoc :: (Arbitrary a, Eq a, Monoid a) => a -> a -> a -> Bool
  monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

  monoidLeftId :: (Monoid a, Eq a) => a -> Bool
  monoidLeftId x = mempty <> x == x

  monoidRightId :: (Monoid a, Eq a) => a -> Bool
  monoidRightId x = x <> mempty == x

  type FirstMappend
    = First' String
    -> First' String
    -> First' String
    -> Bool

  type FstId = First' String -> Bool

  main :: IO ()
  main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftId :: FstId)
    quickCheck (monoidRightId :: FstId)
  