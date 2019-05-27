module Revision where

  import Data.List
  import Data.Maybe (fromMaybe)

  notThe :: String -> Maybe String
  notThe str = 
    case str == "the" of 
      False -> Just str
      _ -> Nothing

  replaceThe :: String -> String
  replaceThe str =
    unwords $ map mapper worded
    where
      worded = words str
      mapper x = fromMaybe "a" (notThe x)
  

  isVowel :: Char -> Bool
  isVowel = (flip elem) "aeiou"
      
  countTheBeforeVowel :: String -> Integer
  countTheBeforeVowel str = go (words str) 0
    where 
      go [] count = count
      go (x:[]) count = count
      go (x:y:xs) count
        | isVowel (head y) && x == "the" = go (y:xs) (count + 1)
        | otherwise = go (y:xs) count

  countVowels :: String -> Integer
  countVowels = toInteger . length . filter isVowel