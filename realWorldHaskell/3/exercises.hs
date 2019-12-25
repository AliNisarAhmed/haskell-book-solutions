module Exercises where

import Data.List (sortBy)

-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

-- 3.1
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- 3.2
calcMean :: (Num a, Fractional a) => [a] -> a
calcMean list = (sum list) / (fromIntegral $ length list)

-- 3.3

listToPal :: [a] -> [a]
listToPal list = list ++ reverse list

-- 3.4

isPal :: Eq a => [a] -> Bool
isPal list = list == reverse list

-- 3.5

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy comparator xs
  where
    comparator x y = compare (length x) (length y)

-- 3.6

intersperse2 :: a -> [[a]] -> [a]
intersperse2 _ [] = []
intersperse2 _ (x:[]) = x
intersperse2 x (y:ys) = y ++ [x] ++ intersperse2 x ys

-- 3.7

data Tree a
    = Node a (Tree a) (Tree a)
    | Empty deriving (Show)

-- 3.8

depth :: Tree a -> Int
depth tree = go tree 0
    where
      go Empty acc = acc
      go (Node _ Empty Empty) acc = acc + 1
      go (Node _ left Empty) acc = go left (acc + 1)
      go (Node _ Empty right) acc = go right (acc + 1)
      go (Node _ left right) acc = (acc + 1) + max (go left acc) (go right acc)

tree1 = Empty
tree2 = Node 1 Empty Empty
tree3 = Node 2 Empty (Node 3 Empty Empty)
tree4 = Node 2 Empty (Node 3 Empty (Node 4 Empty Empty))
tree5 = Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty))

-- 3.9

data Direction = Lefty | Righty | Straight deriving (Eq, Show)

-- 3.10 : https://stackoverflow.com/questions/1211212/how-to-calculate-an-angle-from-three-points

angle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
angle p1 p2 p3 =
  atan2 (y3 - y1) (x3 - x1) - atan2 (y2 - y1) (x2 - x1)
    where
      (x1, y1) = p1
      (x2, y2) = p2
      (x3, y3) = p3

direction :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
direction p1 p2 p3
  | calcAngle < 0 = Righty
  | calcAngle > 0 = Lefty
  | otherwise     = Straight
  where
    calcAngle = angle p1 p2 p3

listOfDirections :: [(Double, Double)] -> [Direction]
listOfDirections (p1:p2:p3:rest) = [direction p1 p2 p3] ++ listOfDirections (p2:p3:rest)
listOfDirections [] = []
listOfDirections (p1:[]) = []
listOfDirections (p1:p2:[]) = []

-- listOfDirections [(0, 2), (1, 2), (2, 2), (2, 3), (2, 4), (3, 4)] -> [Straight, Lefty, Straight, Righty]