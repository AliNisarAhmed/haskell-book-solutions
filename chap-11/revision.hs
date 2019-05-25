{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Revision where
  import Data.List (permutations, nub)

  data Price 
    = Price Integer deriving (Eq, Show)

  data Manufacturer
    = Mini 
    | Mazda
    | Tata 
      deriving (Eq, Show)

  data Airline 
    = Papu
    | Cata
    | Chancy
      deriving (Eq, Show)

  data Vehicle 
    = Car Manufacturer Price
    | Plane Airline deriving (Eq, Show)

  myCar = Car Mini $ Price 14000
  urCar = Car Mazda $ Price 20000
  clownCar = Car Tata $ Price 7000
  doge = Plane Papu

  isCar :: Vehicle -> Bool
  isCar (Car _ _) = True
  isCar _ = False

  isPlane :: Vehicle -> Bool
  isPlane (Plane _) = True
  isPlane _ = False 
  
  areCars :: [Vehicle] -> [Bool]
  areCars = map isCar

  getManu :: Vehicle -> Manufacturer
  getManu (Car manu _) = manu

  ------------------------

  class TooMany a where
    tooMany :: a -> Bool
  
  instance TooMany Int where
    tooMany n = n > 42

  newtype Goats 
    = Goats Int deriving (Eq, Show, TooMany)

  -- instance TooMany Goats where
  --   tooMany (Goats n) = n < 42

  instance TooMany (Int, String) where
    tooMany (n, s) = tooMany n

  instance TooMany (Int, Int) where
    tooMany (i1, i2) = tooMany $ i1 + i2 

  instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x1, x2) = tooMany $ x1 + x2 

  
  ----------------------------------------------

  data FlowerType
    = Gardenia 
    | Daisy
    | Rose 
    | Lilac 
    deriving Show

  type Gardener = String

  data Garden 
    = Garden Gardener FlowerType
      deriving Show

  -- data Garden2
  --   = Gardenia Gardener
  --   | Daisy Gardener
  --   | Rose Gardener
  --   | Lilac Gardener


  -------------------------------------------------------

  data GuessWhat 
    = ChickenButt deriving (Eq, Show)

  data Id a 
    = MkId a deriving (Eq, Show)

  data Product a b 
    = Product a b deriving (Eq, Show)

  data Sum a b
    = First a 
    | Second b
    deriving (Eq, Show)

  data RecordProduct a b 
    = RecordProduct { pfirst :: a
                    , psecond :: b
                    } deriving (Eq, Show)

  -----------------------------------------------------

  data OperatingSystem
    = Linux
    | BSD
    | Mac
    | Windows deriving (Eq, Show)

  data Proglang 
    = Haskell
    | Agda 
    | Elixir
    | Idris 
    deriving (Eq, Show)

  allOperatingSystems :: [OperatingSystem]
  allOperatingSystems = 
    [ Linux
    , BSD
    , Mac
    , Windows
    ]
    
  allLanguages :: [Proglang]
  allLanguages 
    = [Haskell, Agda, Idris, Elixir]

  data Programmer
    = Programmer { os :: OperatingSystem
                  , lang :: Proglang } deriving (Eq, Show)

  allProgrammers1 :: [OperatingSystem] -> [Proglang] -> [Programmer]
  allProgrammers1 os pl = [Programmer x y | x <- os, y <- pl]

  allProgrammers2 :: [OperatingSystem] -> [Proglang] -> [Programmer]
  allProgrammers2 os pl = concat $ zipWith (\a b -> map a b) (map Programmer os) (permutations pl)

  -----------------------------------------------

  data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

  insertb :: Ord a => a -> BinaryTree a -> BinaryTree a
  insertb b Leaf = Node Leaf b Leaf
  insertb b (Node left a right) 
    | b == a = Node left a right
    | b < a = Node (insertb b left) a right
    | b > a = Node left a (insertb b right)

  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

  t1 =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

  t3 = Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))
  t5 = Node (Node (Node Leaf 0 Leaf) 1 Leaf) 3 (Node Leaf 5 (Node Leaf 7 Leaf))

  preorder :: BinaryTree a -> [a]
  preorder Leaf = []
  preorder (Node left val right) = [val] ++ preorder left ++ preorder right

  postorder :: BinaryTree a -> [a]
  postorder Leaf = []
  postorder (Node left val right) = postorder left ++ postorder right ++ [val]

  inorder :: BinaryTree a -> [a]
  inorder Leaf = []
  inorder (Node left val right) = inorder left ++ [val] ++ inorder right

  foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b

  foldTree _ b Leaf = b
  foldTree f b (Node left a right) =
    f a b