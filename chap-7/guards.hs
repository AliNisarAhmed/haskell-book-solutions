module Guards where

  myAbs :: Integer -> Integer
  myAbs x
    | x < 0 = (-x)
    | otherwise = x

  bloodNa :: Integer -> String
  bloodNa x 
    | x < 135 = "Too low"
    | x > 145 = "Too High"
    | otherwise = "Just right"

  isRightTriangle :: (Num a, Eq a) => 
                  a -> a -> a -> String
  isRightTriangle a b c
    | a^2 + b^2 == c^2 = "Right on!"
    | otherwise = "not right"

  avgGrade :: (Fractional a, Ord a) => a -> Char
  avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.7 = 'C'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where
      y = x / 100

  pal :: (Eq a) => [a] -> Bool
  pal xs 
      | xs == reverse xs = True
      | otherwise = False

  numbers :: (Ord a, Num a) => a -> a
  numbers x
      | x < 0 = -1
      | x > 0 = 1
      | otherwise = 0