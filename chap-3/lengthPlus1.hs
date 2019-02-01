module LengthPlus1 where
  x = (+)

  f ::  [a] -> Int
  f xs =
    x w 1
    where 
      w = length xs
