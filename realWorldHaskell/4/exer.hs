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

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f list = foldr step [] list
  where
    step x [] = [[x]]
    step x acc@(y:ys)
      | f x (head y) = (x:y):ys
      | otherwise = y : step x ys


myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr (\x acc -> if pred x then True else acc) False

myCycle :: [a] -> [a]
myCycle [] = error "empty List"
myCycle xs = foldr step [] [1..]
  where
    step _ ys = xs ++ ys

myCycle2 :: [a] -> [a]
myCycle2 = foldr (++) [] . repeat

-- myWords :: [Char] -> [[Char]]
-- myWords [] = []
-- myWords input = fst res
--   where
--     res = foldr step ([], []) input
--     step x (acc, sub) =
--       if x == ' '
--       then ([sub] ++ acc, [])
--       else (acc, [x] ++ sub)

-- great technique, learn and use it!
myWords :: String -> [String]
myWords str = foldr step [] (zip str (tail str ++ " "))
  where
    step (a, b) gs
      | isSpace a = gs
      | isSpace b = [a]: gs
      | otherwise = (a: (head gs)):(tail gs)
    isSpace = (== ' ')

myUnlines :: [[Char]] -> [Char]
myUnlines list = foldr step "" list
  where
    step x acc = (x++"\n") ++ acc