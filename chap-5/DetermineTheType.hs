{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

  example = 1

  e1 = (* 9) 6

  e2 = head [(0, "doge"), (1, "kitteh")]

  e3 = head [ (0 :: Integer, "doge"), (1, "kitteh") ]

  e4 = if False then True else False

  e5 = (length [1, 2, 3, 4]) > (length "TACOCAT")

  x = "Julie"
  y = " <3 "
  z = "Haskell"
  f = x ++ y ++ z