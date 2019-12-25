module SemiGroup9 where

  -- import Test.QuickCheck
  -- import Data.Semigroup

  newtype Combine a b = 
    Combine { unCombine :: (a -> b) } 

  instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

  --    

  newtype Comp a
    = Comp { unComp :: (a -> a) }

  instance (Semigroup a) => Semigroup (Comp a) where
    Comp f <> Comp g = Comp (f <> g)

  -- 

  data Validation a b 
    = Failure a 
    | Success b deriving (Eq, Show)

  instance (Semigroup a) => Semigroup (Validation a b) where
    Success x <> _         = Success x
    _ <> Success y         = Success y
    Failure x <> Failure y = Failure (x <> y)

  main = do
    let 
      failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
