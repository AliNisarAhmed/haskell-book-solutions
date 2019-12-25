module OrdInstance where

  -- data DayOfWeek =
  --   Mon | Tue | Wed | Thu | Fri | Sat | Sun
  --   deriving (Show, Eq, Ord)
  
  -- -- Mon < Tue < Wed < Thu...


  -- we can also make our own Ord

  data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq)

  instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

  -- BELOW WILL NOT COMPILE
  -- check' :: a -> a -> Bool
  -- check' a a' = a == a'

  -- But this will 
  check' :: Ord a => a -> a -> Bool
  check' x y = x == y