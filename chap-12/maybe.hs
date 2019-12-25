module MaybeLibrary where

  isJust :: Maybe a -> Bool
  isJust (Just x) = True
  isJust _ = False

  isNothing :: Maybe a -> Bool
  isNothing Nothing = True
  isNothing _ = False

  mayybee :: b -> (a -> b) -> Maybe a -> b
  mayybee b _ Nothing = b
  mayybee b f (Just x) = f x

  fromMaybe :: a -> Maybe a -> a
  -- fromMaybe x Nothing = x
  -- fromMaybe _ (Just y) = y
  fromMaybe x mb = mayybee x id mb   -- written in terms of the catamorphism above

  listToMaybe :: [a] -> Maybe a
  listToMaybe [] = Nothing
  listToMaybe (x:xs) = Just x

  maybeToList :: Maybe a -> [a]
  maybeToList Nothing = []
  maybeToList (Just x) = [x]

  catMaybes :: [Maybe a] -> [a]
  catMaybes = map f . filter isJust
    where
      f (Just x) = x

  
  flipMaybe :: [Maybe a] -> Maybe [a]
  flipMaybe list =
    case compare (length list) (length cats) of 
      GT -> Nothing
      _ -> Just cats
      where
        cats = catMaybes list