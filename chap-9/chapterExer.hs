module Exercises where

  import Data.Char

  allUpper :: String -> String
  -- allUpper = filter isLower
  allUpper = filter isUpper

  capitalizeFirst :: String -> String
  capitalizeFirst (x:xs) = [toUpper x] ++ xs

  capitalize :: String -> String
  capitalize "" = ""
  capitalize (x:xs) = [toUpper x] ++ capitalize xs

  capitalizeAndReturn :: String -> Char
  capitalizeAndReturn = toUpper . head