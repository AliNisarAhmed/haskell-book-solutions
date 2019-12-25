module FalseMonoid where

  import Control.Monad
  import Data.Monoid
  import Test.QuickCheck

  data Bull
    = Fools
    | Twoo deriving (Eq, Show)

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId a = (mempty <> a) == a

  monoidRightId :: (Eq m, Monoid m) => m -> Bool
  monoidRightId a = (a <> mempty) == a


  instance Arbitrary Bull where
    arbitrary = 
      frequency [(1, return Fools), (1, return Twoo)]

  instance Monoid Bull where
    mempty = Fools

  instance Semigroup Bull where
    _ <> _ = Fools

  type BullMappend = 
    Bull -> Bull -> Bull -> Bull

  main :: IO ()
  main = do
    let ma = monoidAssoc
        mli = monoidLeftId
        mlr = monoidRightId
    quickCheck (ma :: Bull -> Bull -> Bull -> Bool)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mlr :: Bull -> Bool)