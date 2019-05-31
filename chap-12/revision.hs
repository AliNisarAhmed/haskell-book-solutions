module Revision where

  import Data.List
  import Data.Maybe (fromMaybe, fromJust)

  notThe :: String -> Maybe String
  notThe str = 
    case str == "the" of 
      False -> Just str
      _ -> Nothing

  replaceThe :: String -> String
  replaceThe str =
    unwords $ map mapper worded
    where
      worded = words str
      mapper x = fromMaybe "a" (notThe x)
  

  isVowel :: Char -> Bool
  isVowel = (flip elem) vowels
      
  countTheBeforeVowel :: String -> Integer
  countTheBeforeVowel str = go (words str) 0
    where 
      go [] count = count
      go (x:[]) count = count
      go (x:y:xs) count
        | isVowel (head y) && x == "the" = go (y:xs) (count + 1)
        | otherwise = go (y:xs) count

  countVowels :: String -> Integer
  countVowels = toInteger . length . filter isVowel

  --------------

  vowels = "aeiou"

  -- countVowels :: String -> Integer
  -- countVowels = foldr (\x acc -> if elem x vowels then acc + 1 else acc) 0

  newtype Word'
      = Word' String deriving (Eq, Show)

  mkWord :: String -> Maybe Word'
  mkWord str = 
    case count > (toInteger $ length str) - count of 
      True -> Nothing
      _ -> Just $ Word' str
      where
        count = countVowels str


  ------------------------------------------

  data Nat 
    = Zero 
    | Succ Nat
    deriving (Eq, Show)
  
  natToInteger :: Nat -> Integer
  natToInteger Zero = 0
  natToInteger (Succ nat) = 1 + natToInteger nat

  convertToNat :: Integer -> Nat
  convertToNat int 
    | int == 0 = Zero
    | otherwise = Succ $ convertToNat (int - 1)

  integerToNat :: Integer -> Maybe Nat
  integerToNat n = 
    if n < 0 
    then Nothing
    else if n == 0 
    then Just Zero
    else Just $ convertToNat n

  -------------------------------

  isJust :: Maybe a -> Bool
  isJust Nothing = False
  isJust _ = True

  isNothing :: Maybe a -> Bool
  isNothing Nothing = True
  isNothing _ = False

  maybee :: b -> (a -> b) -> Maybe a -> b
  maybee b f Nothing = b
  maybee b f (Just a) = f a

  fromMaybee :: a -> Maybe a -> a
  fromMaybee def = maybee def id 

  listToMaybe :: [a] -> Maybe a
  listToMaybe [] = Nothing
  listToMaybe (x:_) = Just x

  maybeToList :: Maybe a -> [a]
  maybeToList Nothing = []
  maybeToList (Just x) = [x]

  -----------------

  catMaybes :: [Maybe a] -> [a]
  catMaybes = foldr reducer []
    where 
      reducer x acc = 
        case x of
          Just y -> y:acc
          Nothing -> acc

  containsNothing :: [Maybe a] -> Bool
  containsNothing [] = False
  containsNothing (x:xs) = 
    case x of 
      Nothing -> True
      Just _ -> containsNothing xs


  flipMaybe :: [Maybe a] -> Maybe [a]
  flipMaybe list = 
    if containsNothing list == True
      then Nothing
    else 
      Just $ map fromJust list

  ----------------------------------------

  lefts :: [Either a b] -> [a]
  lefts = foldr reducer []
    where 
      reducer x acc = 
        case x of 
          Left a -> a:acc
          Right _ -> acc

  rights :: [Either a b] -> [b]
  rights = foldr reducer []
    where
      reducer (Right x) acc = x:acc
      reducer _ acc = acc

  partitionsEithers :: [Either a b] -> ([a], [b])
  partitionsEithers = foldr reducer ([], [])
    where 
      reducer (Left a) (as, bs) = (a:as, bs)
      reducer (Right b) (as, bs) = (as, b:bs)

  eitherMaybe :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe f (Right b) = Just $ f b
  eitherMaybe f _ = Nothing

  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' aToC _ (Left a) = aToC a
  either' _ bToC (Right b) = bToC b

  eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' f = either' (\_ -> Nothing) (Just . f)

  ---------------------------------

  myIterate :: (a -> a) -> a -> [a]
  myIterate f a = a : myIterate f (f a)

  myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  myUnfoldr f b = 
    case f b of 
      Nothing -> []
      Just (a, c) -> a: myUnfoldr f c

  betterIterate :: (a -> a) -> a -> [a]
  betterIterate f = unfoldr g 
    where
      g a = Just (a, f a)


-----------------------------------------------

  
  data BinaryTree a
    = Leaf 
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

  unfold :: (a -> Maybe(a, b, a)) -> a -> BinaryTree b
  unfold f a = 
    case f a of 
      Nothing -> Leaf
      Just (c1, b, c2) -> Node (unfold f c1) b (unfold f c2)

  treeBuild :: Integer -> BinaryTree Integer
  treeBuild n = unfold f n
    where
      f :: Integer -> Maybe (Integer, Integer, Integer)
      f number 
        | number <= 0 = Nothing
        | otherwise = Just (number - 1, number, number - 1)