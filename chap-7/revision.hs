module Revision where

  f :: (a, b, c)
    -> (d, e, f)
    -> ( (a, d), (c, f) )
  f (a, _, c) (d, _, f) = ((a, d), (c, f))

  funcC x y =
    case x > y of 
      True -> x
      _ -> y

  ifEvenAdd2 n = 
    case even n of 
      True -> n + 2
      _ -> n

  nums x = 
    case compare x 0 of
      LT -> -1
      GT -> 1
      EQ -> 0

  pal :: (Eq a) => [a] -> Bool
  pal xs
    | xs == reverse xs = True
    | otherwise        = False

  numbers :: (Num a, Ord a) => a -> Integer 
  numbers x 
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1


  tensDigit :: Integral a => a -> a
  tensDigit x = d
    where
      xLast = x `div` 10
      d = xLast `mod` 10

  tensDigit2 :: Integral a => a -> a
  tensDigit2 x = d
      where
        xLast = fst $ x `divMod` 10
        d = snd $ xLast `divMod` 10

  hunsD x = d2
    where
      (xLast, _) = x `divMod` 100
      (_, d2) = xLast `divMod` 10

  ------------------------------------------

  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x _ False = x
  foldBool3 _ y True = y

  foldBool :: a -> a -> Bool -> a
  foldBool x y bool = 
    case bool of 
      True -> x
      False -> y

  foldBool2 :: a -> a -> Bool -> a
  foldBool2 x y bool
      | bool == True = x
      | otherwise = y

  -----------------------------------------

  g :: (a -> b) -> (a, c) -> (b, c)
  g aToB (a, c) = (aToB a, c) 

  -----------------------

  roundTrip :: (Show a, Read b) => a -> b
  roundTrip = (read) . show

  main = do
    print $ ((roundTrip 4) :: Int)
    print (id 4)

  --------------------------------------

