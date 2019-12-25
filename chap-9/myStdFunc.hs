module MyStandardFunctions where

  myOr :: [Bool] -> Bool
  myOr [] = False
  myOr (x: xs) = 
    if x == True
      then True
      else myOr xs

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny _ [] = False
  myAny f (x:xs) = 
    if f x == True
      then True
      else myAny f xs

  myElem :: Eq a => a -> [a] -> Bool
  myElem _ [] = False
  myElem x (y: ys) =
    if x == y
      then True
      else myElem x ys

  myElemAny :: Eq a => a -> [a] -> Bool
  myElemAny x = 
    myAny (== x)
    
  myReverse :: [a] -> [a]
  myReverse [] = []
  myReverse (x:xs) = myReverse xs ++ [x]

  squish :: [[a]] -> [a]
  squish [] = []
  squish (x:[]) = x
  squish (x:xs) = x ++ squish xs

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap _ [] = []
  squishMap f (x:xs) = 
    f x ++ squishMap f xs

  squishAgain :: [[a]] -> [a]
  squishAgain = squishMap id

  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy f list = go f list []
    where 
      go _ [] acc = head acc
      go g (x:xs) [] = go g xs [x]
      go g (x:xs) (y:ys) = 
        case g x y of 
          GT -> go g xs [x]
          _ -> go g xs (y:ys)

  myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  myMinimumBy f = myMaximumBy (flip f)

  myMaximum :: (Ord a) => [a] -> a
  myMaximum = myMaximumBy compare

  myMinimum :: (Ord a) => [a] -> a
  myMinimum = myMinimumBy compare

  -- defining our own cons data constructor

  data List a = Nil | Cons a (List a)
  -- e.g. Cons 1 (Cons 2 (Cons 3 Nil))