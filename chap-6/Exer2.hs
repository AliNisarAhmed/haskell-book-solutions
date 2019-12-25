module Exer2 where
  import Data.List (sort)

  -- i :: a  (will not compile as missing Num instance)
  -- i = 1

  -- f :: Num a => a  (Again does not compile as Fractional instance not found)
  -- f = 1.0

  -- f :: Fractional a => a
  -- f = 1.0

  -- f :: RealFrac a => a
  -- f = 1.0

  freud :: Ord a => a -> a
  freud x = x

  freud' :: Int -> Int
  freud' x = x

  myX = 1 :: Int
  -- sigmund :: a -> a
  -- sigmund x = myX

  -- sigmund' :: Num a => a -> a
  -- sigmund' x = myX

  jung :: [Int] -> Int
  jung xs = head (sort xs)

  -- young :: [Char] -> Char
  young :: Ord a => [a] -> a
  young xs = head (sort xs)

  mySort :: [Char] -> [Char]
  mySort = sort

  -- signifier :: [Char] -> Char
  signifier :: Ord a => [a] -> a
  signifier xs = head (mySort xs)