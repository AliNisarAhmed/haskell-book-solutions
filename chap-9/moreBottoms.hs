module MoreBottoms where

  import Data.Bool (bool)
  -- same bool function from Data.Bool
  -- or the one we implemented in Chap-7 foldBool 

  foldBool :: (Num a, Eq a) => [a] -> [a]
  foldBool = map (\x -> bool x (-x) (x == 3))