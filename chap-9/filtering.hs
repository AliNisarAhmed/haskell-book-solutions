module Filtering where

  f :: (Num a, Eq a, Integral a) => [a] -> [a]
  -- f = filter (\x -> rem x 3 == 0)
  f = filter ( (== 0) . ((flip rem) 3))
  
  numOfMultiples :: Integral a => [a] -> Int
  numOfMultiples = length . f

  myFilter :: String -> [String]
  myFilter = filter (\x -> not (elem x ["the", "an", "a"])) . words
  
  sentence = "the brown fox, an insecure lady and a burly man"