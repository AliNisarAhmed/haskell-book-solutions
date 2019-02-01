module AbsoluteValue where

  abs :: Integer -> Integer
  abs x = 
    if x > 0 
    then x
    else (-1) * x

  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f (a, b) (c, d) = ((b, d), (a, c))