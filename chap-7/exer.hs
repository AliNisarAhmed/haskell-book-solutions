module Exer where
  
  -- tensDigit :: Integral a => a -> a
  -- tensDigit x = d
  --   where
  --     xLast = x `div` 10
  --     d = xLast `mod` 10


  tensDigit :: Integral a => a -> a
  tensDigit x = d
    where
      (xLast, _) = divMod x 10
      (_, d) = divMod xLast 10

  hunsD :: Integral a => a -> a
  hunsD x = d
    where
      (xLast, _) = divMod x 100
      (_, d) = divMod xLast 10

  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x _ False = x
  foldBool3 _ y True = y

  foldBool1 :: a -> a -> Bool -> a
  foldBool1 x y bool =
    case bool of 
      False -> x
      True -> y

  foldBool2 :: a -> a -> Bool -> a
  foldBool2 x y bool 
      | bool == False = x
      | bool == True = y

  g :: (a -> b) -> (a, c) -> (b, c)
  g f (a, c) =
    (f a, c)