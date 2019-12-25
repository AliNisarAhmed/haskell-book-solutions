module Revision where

  import Data.Time

  -- myAny :: (a -> Bool) -> [a] -> Bool
  -- myAny f xs = foldr (\x acc -> f x || acc) False xs

  data DatabaseItem
    = DBString String
    | DBNumber Integer
    | DBDate UTCTime
    deriving (Eq, Ord, Show)

  theDatabase :: [DatabaseItem]
  theDatabase = 
    [ DBDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DBNumber 9001
    , DBString "Hello, world!"
    , DBDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

  -- filterDBDate :: [DatabaseItem] -> [UTCTime]
  -- filterDBDate = map mapFunc . filter filterDate 
  --   where
  --     filterDate (DBDate xs) = True
  --     filterDate _ = False
  --     mapFunc (DBDate xs) = xs

  filterDBDate :: [DatabaseItem] -> [UTCTime]
  filterDBDate = foldr folderFunc []
    where
      folderFunc :: DatabaseItem -> [UTCTime] -> [UTCTime]
      folderFunc (DBDate utc) acc = utc : acc
      folderFunc _ acc = acc

  list = [ DBString "Ali"
          , DBNumber 24
          , DBString "Ahmed"
          , DBNumber 98
          , DBDate (UTCTime (fromGregorian 2019 5 15) (secondsToDiffTime 34123))
          , DBDate (UTCTime (fromGregorian 2019 4 8)
          (secondsToDiffTime 12345))
        ]

  filterDBNumber :: [DatabaseItem] -> [Integer]
  filterDBNumber = foldr folderFunc []
      where
        folderFunc :: DatabaseItem -> [Integer] -> [Integer]
        folderFunc (DBNumber num) acc = num : acc
        folderFunc _ acc = acc
      

  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent = maximum . filterDBDate

  sumDB :: [DatabaseItem] -> Integer
  sumDB = sum . filterDBNumber

  avgDB :: [DatabaseItem] -> Double
  avgDB xs = (fromIntegral $ sum filtered) / (fromIntegral $ length $ filtered) 
    where
      filtered = filterDBNumber xs

  fibs = 1 : scanl (+) 1 fibs
  fibsN x = fibs !! x

  --------------------------------------------

  stops = "pbtdkg"
  vowels = "aeiou"

  tuples3 :: String -> String -> [(Char, Char, Char)]
  tuples3 stops vowels = 
    [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

  nouns = ["Ali", "Samrah", "Noshi", "Cat", "Dog", "Sky", "Yellow"]
  verbs = ["loves", "hates", "adores", "helps", "follows"]

  nVn :: [String] -> [String] -> [String]
  nVn n v = 
    [x ++ " " ++ y ++ " " ++ z | x <- n, y <- v, z <- n]


  -----------------------------------

  -- Finds the average length of words in a sentence
  seekritFunc :: String -> Double
  seekritFunc x = 
    (fromIntegral $ sum (map length (words x))) / (fromIntegral $ length (words x))

  ----------------------

  myOr :: [Bool] -> Bool
  myOr = foldr (||) False

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny f = foldl (\acc x -> f x || acc) False

  myElem :: Eq a => a -> [a] -> Bool
  myElem x = foldl (\acc x -> )