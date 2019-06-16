module Lib where
  import Data.List (intersperse)

  digitToWord :: Int -> String
  digitToWord x 
        | x == 0 = "zero"
        | x == 1 = "one"
        | x == 2 = "two"
        | x == 3 = "three"
        | x == 4 = "four"
        | x == 5 = "five"
        | x == 6 = "six"
        | x == 7 = "seven"
        | x == 8 = "eight"
        | x == 9 = "nine"

  digits :: Int -> [Int]
  digits x = go x []
    where 
      go num list 
        | divResult == 0 = modResult : list
        | otherwise = go (divResult) (modResult : list)
          where 
            divResult = div num 10
            modResult = mod num 10

  wordNumber :: Int -> String
  -- wordNumber num = concat addHyphen
  --   where
  --     addHyphen = intersperse "-" listString
  --     listString = map digitToWord listNums
  --     listNums = digits num

  wordNumber = concat . intersperse "-" . map digitToWord . digits