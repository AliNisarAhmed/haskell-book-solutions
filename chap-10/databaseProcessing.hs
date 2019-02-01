module DBProcessing where
  import Data.Time

  data DatabaseItem 
    = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

  theDatabase :: [DatabaseItem]
  theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    , DbNumber 1234
    , DbNumber 2341
    , DbNumber 9999
    ]

  filterDbDate :: [DatabaseItem] -> [UTCTime]
  -- filterDbDate xs = map (\(DbDate utc) -> utc) $ filter filterFunc xs
  --   where 
  --     filterFunc dbDate =
  --       case dbDate of
  --         DbDate x -> True
  --         _ -> False
  filterDbDate db = foldr f [] db
    where
      f (DbDate a) b = a : b
      f _ b = b

  filterDbNumber :: [DatabaseItem] -> [Integer]
  -- filterDbNumber xs = map (\(DbNumber x) -> x) $ filter filterFunc xs
  --   where 
  --     filterFunc dbItem =
  --       case dbItem of 
  --         DbNumber num -> True
  --         _ -> False
  filterDbNumber db = foldr f [] db
    where 
      f (DbNumber a) b = a : b
      f _ b = b

  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent = f . filterDbDate
      where 
        f (x:xs) = foldr (\a b -> if a > b then a else b) x xs

  sumDb :: [DatabaseItem] -> Integer
  sumDb = f . filterDbNumber
      where 
        f xs = foldr (+) 0 xs 

  avgDb :: [DatabaseItem] -> Double
  avgDb = f . filterDbNumber
    where
      f xs = (fromIntegral (foldr (+) 0 xs)) / (fromIntegral (length xs))