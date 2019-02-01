module Cipher where

  import Data.Char

  caesar :: Int -> String -> String
  caesar shift =  map (chr . rounder . (+ shift) . subtract 97 . ord) 
    where 
      rounder :: Int -> Int
      rounder x  
        | x >= 26 = (mod x 26) + 97
        | otherwise = x + 97

  unCaesar :: Int -> String -> String
  unCaesar unshift = map (chr . rounder . subtract unshift . subtract 97 . ord ) 
      where 
        rounder :: Int -> Int
        rounder x
          | x < 0 = (mod x 26) + 97
          | otherwise = x + 97 
