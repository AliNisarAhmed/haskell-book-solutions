module SplitWith where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:xs) = Just $ myInit xs
  where
    myInit (x:[]) = [x]
    myInit (x:xs) = x : myInit xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred xs
  | null last = [first]
  | null first = splitWith pred (tail last)
  | otherwise = first : splitWith pred (tail last)
  where
    (first, last) = span pred xs