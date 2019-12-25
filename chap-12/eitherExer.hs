module EitherExer where

  lefts' :: [Either a b] -> [a]
  lefts' = foldr f []
    where 
      f (Left x) b = [x] ++ b
      f _ b = b

  rights' :: [Either a b] -> [b]
  rights' = foldr f []
    where 
      f (Right x) b = x : b
      f _ b = b 

  partitionsEithers' :: [Either a b] -> ([a], [b])
  partitionsEithers' = foldr f ([], [])
    where 
      f (Left x) (y, z) = (x : y, z)
      f (Right x) (y, z) = (y, x : z)

  eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' f (Left a) = Nothing
  eitherMaybe' f (Right b) = Just $ f b
  
  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' aToC _ (Left a) = aToC a
  either' _ bToC (Right b) = bToC b

  eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe'' _ (Left a) = Nothing
  eitherMaybe'' f (Right b) = Just $ either' id f (Right b)