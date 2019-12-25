module EnumFromToFunctions where

  eftBool :: Bool -> Bool -> [Bool]
  eftBool False True = [False, True]
  eftBool True True = [True]
  eftBool False False = [False]
  eftBool _ _ = []

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd x y
    | x == y = [x]
    | x == EQ && y == GT = [EQ, GT]
    | x == LT && y == GT = [LT, EQ, GT]
    | x == LT && y == EQ = [LT, EQ]
    | otherwise = []

  eftInt :: Int -> Int -> [Int]
  eftInt x y
    | x > y = []
    | x == y = [x]
    | otherwise = [x] ++ eftInt (succ x) y

  eftChar :: Char -> Char -> [Char]
  eftChar x y
    | x > y = []
    | x == y = [x]
    | otherwise = [x] ++ eftChar (succ x) y