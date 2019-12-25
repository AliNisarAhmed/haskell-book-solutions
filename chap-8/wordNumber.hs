module WordNumber where

  import Data.List (intersperse)

  digitToWord :: Int -> String
  digitToWord 0 = "zero"
  digitToWord 1 = "one"
  digitToWord 2 = "two"
  digitToWord 3 = "three"
  digitToWord 4 = "four"
  digitToWord 5 = "five"
  digitToWord 6 = "six"
  digitToWord 7 = "seven"
  digitToWord 8 = "eight"
  digitToWord 9 = "nine"

  digits :: Int -> [Int]
  digits = (:[])

  mapDigitToWord :: Int -> [String]
  mapDigitToWord = map digitToWord . digits 

  listOfWords :: Int -> [String]
  listOfWords int = go $ divMod int 10
    where
      go (0, x) = mapDigitToWord x
      go (x, y) = listOfWords x ++ mapDigitToWord y

  wordNumber :: Int -> String
  wordNumber = concat . intersperse "-" . listOfWords