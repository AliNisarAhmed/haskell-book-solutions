module Dodgy where

  dodgy :: Num a => a -> a -> a
  dodgy x y =
    x + y * 10
  
  onIsOne :: Num a => a -> a
  onIsOne = dodgy 1

  oneIsTwo :: Num a => a -> a
  oneIsTwo = (flip dodgy) 2