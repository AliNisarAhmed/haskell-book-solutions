module ReWriteFuncs where

  myAnd :: [Bool] -> Bool
  -- myAnd [] = True
  -- myAnd (x:xs) =
  --   case x of 
  --     False -> False
  --     _ -> myAnd xs

  -- myAnd [] = True
  -- myAnd (x:xs) = x && myAnd xs
      

  -- myAnd = 
  --   foldr (\a b -> 
  --     if a == False then False else b) True
      
  myAnd = foldr (&&) True

  -----------------------------------------------

  myOr :: [Bool] -> Bool
  -- myOr [] = True 
  -- myOr (x:xs) = 
  --   if x == True
  --     then True
  --     else myOr xs

  -- myOr [] = True
  -- myOr (x:xs) = x || myOr xs

  -- myOr = foldr (\a b -> 
  --   if a == True then True else b) False

  myOr = foldr (||) False

  --------------------------------------------

  myAny :: (a -> Bool) -> [a] -> Bool
  -- myAny f [] = False
  -- myAny f (x:xs) =
  --   if f x == True then True else myAny f xs

  myAny f = 
    foldr (\a b -> if f a == True then True else b) False

  -- myAny f = foldr (\a b -> f a || b) False

  -----------------------------------------------------

  myElem :: Eq a => a -> [a] -> Bool
  -- recursive
  -- myElem a (x:xs) = 
  --   if x == a 
  --     then True
  --     else myElem a xs

  -- foldr
  -- myElem x = foldr (\a b -> a == x || b) False

  -- using ANY
  myElem x = any (==x)


  ----------------------------------------------------------

  myReverse :: [a] -> [a]
  -- myReverse [] = []
  -- myReverse (x:xs) = myReverse xs ++ [x]

  myReverse = foldr (\a b -> b ++ [a]) []

  -- OR
  -- myReverse = foldl (flip (:)) []


  -------------------------------------------------------------

  myMap :: (a -> b) -> [a] -> [b]
  myMap f = foldr (\a b -> f a : b) []

  -- pointfree
  -- myMap f = foldr ((:) . f) []

  --------------------------------------------------------------

  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter f =
    foldr (\a b -> if f a == True then a:b else b) []  


  -----------------------------------------------------------------

  squish :: [[a]] -> [a]
  squish = foldr (\a b -> a ++ b) []

  -------------------------------------------------------------

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f = foldr (\a b -> (f a) ++ b) []

  --------------------------------------------------------------

  squishAgain :: [[a]] -> [a]
  squishAgain = squishMap id

  ----------------------------------------------------------------

  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy f (y:ys)= foldl foldingFunc y ys
    where
      foldingFunc b a =
        case f b a of 
          LT -> a
          _ -> b