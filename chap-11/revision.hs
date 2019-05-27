{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Revision where
  import Data.List (permutations, nub, elemIndex)
  import Data.Char (chr, ord, toUpper, toLower, isUpper, isLower)
  import Data.Maybe (fromJust, fromMaybe)

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
  foldTree f b bt = 
    foldr f b list 
      where
        list = postorder bt

  -----------------------------------

  caesar :: Int -> String -> String
  caesar _ [] = []
  caesar shift (x:xs) = 
    (chr . (+ 97) . (`mod` 26) . (+ shift) . subtract 97 . ord $ x): caesar shift xs

  uncaesar :: Int -> String -> String
  uncaesar _ [] = []
  uncaesar unshift (x:xs) = 
    (chr . (+97) . (`mod` 26) . (subtract unshift) . (subtract 97) . ord $ x ) : uncaesar unshift xs

  -- vigenere :: String -> String -> String
  vigenere text code = 
    let 
      digits = map ord text
      cycled = cycle digits
      zipped = zipWith caesar cycled (words text)
    in 
      zipped
  
  isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
  isSubseqOf [] _ = True
  isSubseqOf _ [] = False
  isSubseqOf subset@(x:xs) total@(y:ys)
    | x == y = isSubseqOf xs ys
    | x /= y = isSubseqOf subset ys

  capitalizeWords :: String -> [(String, String)]
  capitalizeWords sentence = 
    foldr reducer [] wordsList
    where
      wordsList = words sentence
      reducer word@(y:ys) acc = (word, toUpper y: ys) : acc


  -----------------------------

  capitalizeWord :: String -> String
  capitalizeWord (x:xs) = toUpper x : xs

  capitalizePara :: String -> String
  capitalizePara str = go sentence ""  
    where
      sentence = words str
      go [] acc = acc
      go sent@(firstW: secondW: rest) acc = 
        let 
          lastChar = firstW !! (length firstW - 1)
        in
          case lastChar == '.' of 
            True -> go rest (acc ++ firstW ++ (capitalizeWord secondW))
            _ -> go rest (acc ++ firstW ++ secondW)


  -------------------------------------------

  data DaPhone =
    DaPhone [(Char, String)] deriving Show

  phone :: DaPhone
  phone = DaPhone [
      ('*', "^")
    , ('#', ".,")
    , ('0', "+_")
    , ('1', "")
    , ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyz")
    ]

  type Digit = Char
  type Presses = Int

  findChar :: Char -> DaPhone -> [(Char, String)]
  findChar char phone = 
    case isLower char of 
      True -> go char phone []
      _ -> go (toLower char) phone [('*', "^")]
      where 
        go c (DaPhone (x:xs)) acc
          | elem c (snd x) = acc ++ [x]
          | otherwise = go c (DaPhone xs) acc

          
  getTaps :: Char -> (Char, String) -> (Digit, Presses)
  getTaps char (c, str)
    | char == '*' = ('*', 1)
    | char == ' ' = ('0', 1)
    | otherwise = (c, (fromMaybe 0 $ elemIndex (toLower char) str) + 1)


  reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
  reverseTaps phone char = 
    let
      chars = findChar char phone
      taps = map (getTaps char) chars
    in
      taps

  cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
  cellPhonesDead phone str = 
    concat $ map (reverseTaps phone) str

  fingerTaps :: [(Digit, Presses)] -> Presses
  fingerTaps = foldr (\(d, p) acc -> p + acc) 0

  convo :: [String]
  convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

  occurences :: String -> Char -> Int
  occurences str char = foldr (\x acc -> if x == char then acc + 1 else acc) 0 str

  mostPopularLetter :: String -> Char
  mostPopularLetter str@(x:xs) = foldr (\x acc -> if occurences str x > occurences str acc && x /= ' ' then x else acc) x str

  costOfMostPopular :: String -> Int
  costOfMostPopular str@(s:ss) = (* occur) $ fingerTaps $ reverseTaps phone mostPopular
    where
      mostPopular = mostPopularLetter str 
      occur = occurences str mostPopular 

  --------------------------------------------------------------

  data Expr
    = Lit Integer
    | Add Expr Expr

  eval :: Expr -> Integer
  eval (Lit int) = int
  eval (Add expr1 expr2) = eval expr1 + eval expr2

  printExpr :: Expr -> String
  printExpr (Lit int) = show int
  printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2