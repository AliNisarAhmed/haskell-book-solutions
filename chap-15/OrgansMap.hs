module OrganMap where

  import qualified Data.Map.Strict as M

  data Organ = Heart | Brain | Kidney | Spleen deriving (Eq, Show, Ord)

  organs :: [Organ]
  organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

  ids :: [Int]
  ids = [2, 7, 13, 14, 21, 24]

  organPairs :: [(Int, Organ)]
  organPairs = zip ids organs

  organCatalog :: M.Map Int Organ
  organCatalog = M.fromList organPairs

  count :: Organ -> [Organ] -> Int
  count org list = foldr f 0 list
    where
      f organ int = 
        if organ == org then
          int + 1
          else int
          
  countOrgans :: [Organ] -> [Organ] -> [(Organ, Int)]
  countOrgans std list = 
    map f std
      where
        f organ = (organ, count organ list)

  organInventory :: M.Map Organ Int
  organInventory = 
    M.fromList (countOrgans [Heart, Brain, Spleen, Kidney] organs)

  possibleDrawers :: [Int]
  possibleDrawers = [1..50]

  getDrawerContents :: [Int] -> M.Map Int Organ -> [Maybe Organ]
  getDrawerContents numbers catalog = 
    map (\k -> M.lookup k catalog) numbers

  availableOrgans :: [Maybe Organ]
  availableOrgans = getDrawerContents possibleDrawers organCatalog

  countOrgan :: Organ -> [Maybe Organ] -> Int
  countOrgan organ available =
    length (filter (\x -> x == Just organ) available)