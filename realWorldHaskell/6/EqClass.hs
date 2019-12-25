module EqClass where

import Data.Char (isSpace)
import JsonHaskell

data Color = Red | Green | Blue

class BasicEq a where
  isEqual :: a -> a -> Bool
  -- providing default implementation so users only have to implement one
  isEqual x y = not (isNotEqual x y)

  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

instance Show Color where
  show Red = "Color: Red"
  show Green = "Color: Green"
  show Blue = "Color: Blue"

instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where
      tryParse [] = []
      tryParse ((attempt, result): xs) =
        if (take (length attempt) value) == attempt
          then [(result, drop (length $ attempt) value)]
          else tryParse xs