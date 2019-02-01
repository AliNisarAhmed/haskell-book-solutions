module CaseExpr where

  functionC :: Ord a => a -> a -> a
  functionC x y = 
    case x > y of 
      True -> x
      _ -> y

  ifEvenAdd2 :: Integral a => a -> a
  ifEvenAdd2 n = 
    case even n of 
      True -> n + 2
      _ -> n

  nums :: (Integral a) => a -> Int
  nums x = 
    case compare x 0 of 
      LT -> -1
      GT -> 1
      EQ -> 0