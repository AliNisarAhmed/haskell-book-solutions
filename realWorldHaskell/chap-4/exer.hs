module Exer where

import Data.Char (ord, digitToInt, isDigit)
import Data.List (foldl', groupBy)

asInt :: String -> Int
asInt list@(x:xs)
  | x == '-' = (-1) * asInt xs
  | otherwise = foldl' step 0 list
    where
      step acc x = acc * 10 + digitToInt x


asInt2 :: String -> Either String Int
asInt2 list@(x:xs)
  | x == '-' = fmap ((*) (-1)) $ asInt2 xs
  | otherwise = foldl' step (Right 0) list
  where
    step acc x
      | isDigit x = fmap (\y -> y * 10 + digitToInt x) acc
      | otherwise = Left $ "non-digit: " ++ [x]


myConcat :: [[a]] -> [a]
myConcat list = foldr (++) [] list

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f list = go [] list
    where
      go acc (x:xs)
        | f x = go (acc ++ [x]) xs
        | otherwise = acc

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 f list = foldr step [] list
  where
    step x acc
      | f x = x : acc
      | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f list = foldr step [] list
    where
      step x acc
        | f x = acc
        | otherwise = x : acc