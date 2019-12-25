module Warmup where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return $ (,) a b

tupled2 :: [Char] -> ([Char], [Char])
tupled2 = cap >>= (\x -> rev >>= (\y -> return (x, y)))