module Revision where

  rvrs :: String -> String
  rvrs [] = []
  rvrs str = rvrs (drop 1 str) ++ (take 1 str)

  x = (+)

  f xs = w `x` 1
    where
      w = length xs