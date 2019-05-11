module Revision where

  import Data.List (intersperse)

  addTo :: (Eq a, Num a) => a -> a
  addTo 1 = 1
  addTo n = n + addTo (n - 1)


  mult :: (Integral a) => a -> a -> a
  mult x 1 = x
  mult x y = x + mult x (y - 1)

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy num denom = go num denom 0
    where
      go n d count 
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)
    
  dividedBy2 :: Integral a => a -> a -> Maybe (a, a)
  dividedBy2 num denom = go num denom 0
      where 
        go n d count 
          | n < d && (n < 0 || d < 0) = Just ((-1) * count, n)
          | n < d && (n >= 0 && d >= 0) = Just (count, n)
          | d == 0 = Nothing
          | otherwise = go (n - d) d (count + 1)

  --------------------------------------------------------------------

  mc91 :: (Num a, Ord a) => a -> a
  mc91 x 
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11

  ------------------------------

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

  