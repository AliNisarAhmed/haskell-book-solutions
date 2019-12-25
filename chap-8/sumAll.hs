module SumAll where

  sumAll :: (Eq a, Num a) => a -> a
  sumAll n 
    | n == 0 = 0
    | otherwise = n + sumAll (n - 1)

  -- sumAll 0 = 0
  -- sumAll n = n + sumAll (n - 1)