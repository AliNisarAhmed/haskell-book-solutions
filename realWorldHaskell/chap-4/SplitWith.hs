module SplitWith where

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred xs
  | null last = [first]
  | null first = splitWith pred (tail last)
  | otherwise = first : splitWith pred (tail last)
  where
    (first, last) = span pred xs