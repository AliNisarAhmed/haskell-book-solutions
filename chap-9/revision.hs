module Revision where

  import Data.Char

  eftBool :: Bool -> Bool -> [Bool]
  eftBool True True = [True]
  eftBool True False = []
  eftBool False True = [False, True]
  eftBool False False = [False]

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd ord1 ord2
    | ord1 == ord2 = [ord1]
    | ord1 == LT && ord2 == GT = [LT, EQ, GT]
    | ord1 == LT && ord2 == EQ = [LT, EQ]
    | ord1 == EQ && ord2 == GT = [EQ, GT]
    | otherwise = []

  eftInt :: Int -> Int -> [Int]
  eftInt int1 int2
    | int1 == int2 = [int1]
    | int1 > int2 = []
    | otherwise = int1 : eftInt (succ int1) int2

  eftChar :: Char -> Char -> [Char]
  eftChar c1 c2 
    | c1 == c2 = [c1]
    | c1 > c2 = []
    | otherwise = c1 : eftChar (succ c1) c2

    ------------------------------------------------------

  myWords :: String -> [String]
  myWords [] = []
  myWords str = 
    [partialTake str] ++ myWords (partialDrop str) 
    where
      partialTake = takeWhile (/= ' ')
      partialDrop = dropWhile (== ' ') . dropWhile (/= ' ') 

  ------------------------------------------------------------

  firstSen = "Tyger Tyger, burning bright\n"
  secondSen = "In the forests of the night\n"
  thirdSen = "What immortal hand or eye\n"
  fourthSen = "Could frame thy fearful symmetry?"

  sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

  myLines :: String -> [String] 
  myLines [] = []
  myLines str = 
    [partialTake str] ++ myLines (partialDrop str)
      where
        partialTake = takeWhile (/= '\n')
        partialDrop = drop 1 . dropWhile (/= '\n')

 -- ---------------------------------

  myFunc :: String -> Char -> [String]
  myFunc [] _ = []
  myFunc str sep = 
   [partialTake str] ++ myFunc (partialDrop str) sep
   where
    partialTake = takeWhile (/= sep)
    partialDrop = drop 1 . dropWhile (/= sep)

  ---------------------------------------

  mySqr = [x^2 | x <- [1..5]]
  myCube = [y^3 | y <- [1..5]]

  myTuples = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]



  -------------------------------

  itIsMystery :: String -> [Bool]
  itIsMystery xs = map (\x -> elem x "aeiou") xs


  ------------------

  removeArticle :: String -> [String]
  removeArticle = filter (\x -> not $ elem x ["a", "an", "the"]) . words 

  -- --------------

  -- myZip :: [a] -> [b] -> [(a, b)]
  -- myZip [] _ = []
  -- myZip _ [] = []
  -- myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

  myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  myZipWith _ [] _ = []
  myZipWith _ _ [] = []
  myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

  myZip :: [a] -> [b] -> [(a, b)]
  myZip = myZipWith (,) 


----------------------------------------------------
  capitalizeFirst :: String -> String
  capitalizeFirst (x:xs) = toUpper x : xs

  capitalizeWhole :: String -> String
  capitalizeWhole [] = []
  capitalizeWhole (x:xs) = toUpper x : capitalizeWhole xs

  capital :: String -> Char
  capital = toUpper . head 


  ----------------------

  caesar :: Int -> String -> String
  caesar _ [] = []
  caesar shift (x:xs) = 
    (chr . (+ 97) . (`mod` 26) . (+ shift) . subtract 97 . ord $ x): caesar shift xs

  uncaesar :: Int -> String -> String
  uncaesar _ [] = []
  uncaesar unshift (x:xs) = 
    (chr . (+97) . (`mod` 26) . (subtract unshift) . (subtract 97) . ord $ x ) : uncaesar unshift xs

  -----------------------------------

  myOr :: [Bool] -> Bool
  myOr [] = False
  myOr (x:xs) = 
    case x of 
      True -> True
      False -> myOr xs

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny _ [] = False
  myAny f (x:xs) = 
    case f x of 
      True -> True
      False -> myAny f xs

  myElem :: Eq a => a -> [a] -> Bool
  myElem _ [] = False
  myElem y (x:xs) = 
    x == y || myElem y xs

  myElem2 :: Eq a => a -> [a] -> Bool
  myElem2 = myAny . (==)

  myReverse :: [a] -> [a]
  myReverse [] = []
  myReverse (x:xs) = myReverse xs ++ [x]

  squish :: [[a]] -> [a]
  squish [] = []
  squish (x:xs) = x ++ squish xs

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f = squish . map f

  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy func (x:xs) = go func xs x
    where 
      go _ [] c = c
      go f (y:ys) c = 
        case f y c of 
          GT -> go f ys y
          _ -> go f ys c
  
  myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  myMinimumBy f = myMaximumBy (flip f)
  
  myMaximum :: (Ord a) => [a] -> a
  myMaximum = myMaximumBy compare

  myMinimum :: (Ord a) => [a] -> a
  myMinimum = myMinimumBy compare