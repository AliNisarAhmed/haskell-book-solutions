module Cipher where

  import Data.Char
  import System.IO

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

  main :: IO [Char]
  main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Enter a shift: "
    shift <- getLine
    putStrLn "Enter your string to encode: "
    str <- getLine
    putStrLn "Here is your encoded String: "
    return (caesar (read shift) str)
