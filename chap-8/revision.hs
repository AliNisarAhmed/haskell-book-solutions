module Revision where

  addTo :: (Eq a, Num a) => a -> a
  addTo 1 = 1
  addTo n = n + addTo (n - 1)


  mult :: (Integral a) => a -> a -> a
  mult x 1 = x
  mult x y = x + mult x (y - 1)