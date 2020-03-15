{-# LANGUAGE ViewPatterns #-}

module ViewPatterns where

someList :: [(String, Int)]
someList = [("Edmonton", 1), ("Calgary", 2), ("Toronto", 3)]

findIn :: Eq a => [(a, b)] -> a -> Maybe b
findIn = flip lookup

getOrDefault :: String -> Int
getOrDefault name =
  case mRes of
    Just val -> val
    Nothing  -> 5
    where mRes = findIn someList name

-- Same function written with ViewPatterns
getOrDefault2 :: String -> Int
getOrDefault2 (findIn someList -> Just n) = n
getOrDefault2 _                           = 5

-- View patterns can also make use of variables declared before them in the list of arguments to a function. In this case, the list variable:

getOrDefaultList :: [(String, Int)] -> String -> Int
getOrDefaultList list name =
  case mRes of
    Just n  -> n
    Nothing -> 5
    where
      mRes = findIn list name

getOrDefaultList2 :: [(String, Int)] -> String -> Int
getOrDefaultList2 list (findIn list -> Just n) = n
getOrDefaultList2 _ _                          = 5
