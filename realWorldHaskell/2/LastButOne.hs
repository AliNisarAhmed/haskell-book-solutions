module LastButOne where

lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne (x:[y]) = Just x
lastButOne (x:xs) = lastButOne xs