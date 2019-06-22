module Revision where

  import Control.Monad
  import Data.Monoid
  import Test.QuickCheck

  data Optional a 
    = Nada 
    | Only a 
    deriving (Eq, Show)

  instance (Semigroup a) => Semigroup (Optional a) where
    (<>) Nada x = x
    (<>) y Nada = y
    (<>) (Only a) (Only b) = Only (a <> b)

  instance (Monoid a) => Monoid (Optional a) where
    mempty = Nada


  --------------------

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId a = (mempty <> a) == a

  monoidRightId :: (Eq a, Monoid a) => a -> Bool
  monoidRightId a = (a <> mempty) == a 

  data Bull 
    = Fools 
    | Twoo
    deriving (Eq, Show)

  instance Arbitrary Bull where
    arbitrary = 
      frequency [ (1, return Fools) 
                , (1, return Twoo) ]
  
  instance Semigroup Bull where
    (<>) _ _ = Fools

  instance Monoid Bull where
    mempty = Fools

  type BullMappend = 
    Bull -> Bull -> Bull -> Bool

  main :: IO ()
  main = do 
    let 
      ma = monoidAssoc
      mli = monoidLeftId
      mri = monoidRightId
    quickCheck (ma :: BullMappend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mri :: Bull -> Bool)