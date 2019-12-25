module Revision where
  import Data.List as L

  data Trivial
    = Trivial

  instance Eq Trivial where
    Trivial == Trivial = True

  data DayOfWeek 
    = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  
  data Date 
    = Date DayOfWeek Int

  instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _     = False

  instance Eq Date where
    (==) (Date dayOfWeek1 day1) (Date dayOfWeek2 day2) = 
      dayOfWeek1 == dayOfWeek2 && day1 == day2

  -- ===================================================================

  data TisAnInteger
      = TisAn Integer
  
  instance Eq TisAnInteger where
    (==) (TisAn int1) (TisAn int2) = int1 == int2

  -----------------------------

  data TwoIntegers 
    = Two Integer Integer
  
  instance Eq TwoIntegers where
    (==) (Two int1 int2) (Two int3 int4) = int1 == int3 && int2 == int4

  ----------------------------

  data StringOrInt 
    = TisAnInt Int
    | TisAString String
  
  instance Eq StringOrInt where
    (==) (TisAnInt int1) (TisAnInt int2) = int1 == int2
    (==) (TisAString str1) (TisAString str2) = str1 == str2
    (==) _ _ = False

  ----------------------------

  data Pair a 
    = Pair a a

  instance (Eq a) => Eq (Pair a) where
    (==) (Pair x1 y1) (Pair x2 y2) = x1 == x2 && y1 == y2

  ----------------------------

  data Tuple a b
    = Tuple a b
  
  instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x1 y1) (Tuple x2 y2) = x1 == x2 && y1 == y2

  ----------------------------

  data Which a 
    = ThisOne a 
    | ThatOne a
  
  instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _ = False

  -----------------------------

  data EitherOr a b
    = Hello a
    | Goodbye b

  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False

  -- ===================================================

  data Person
    = Person Bool deriving (Show)
  
  printPerson :: Person -> IO ()
  printPerson p = putStrLn (show p)

  -------------------------------------

  data Mood 
    = Blah 
    | Woot deriving (Show, Eq)
  
  settleDown x =
      if x == Woot
        then Blah
        else x

  --------------------------------------

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence 
    = Sentence Subject Verb Object deriving (Eq, Show)

  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "Loves" "Dogs"

  ------------------------------------------------------

  data Rocks 
    = Rocks String deriving (Eq, Show)

  data Yeah 
        = Yeah Bool deriving (Eq, Show)
  data Papu
    = Papu Rocks Yeah 
        deriving (Eq, Show)
  
  phew = Papu (Rocks "chases") (Yeah True)
  
  equalityForAll :: Papu -> Papu -> Bool
  equalityForAll p p2 = p == p2

  -------------------------------------

  f :: RealFrac a => a
  f = 1.0

  freud :: Ord a => a -> a
  freud x = x

  freud2 :: Int -> Int
  freud2 x = x

  -- myX = 1 :: Int
  -- sigmund :: a -> a
  -- sigmund x = myX

  -- sigmund2 :: Num a => a -> a
  -- sigmund2 x = myX

  jung :: [Int] -> Int
  jung xs = head (L.sort xs)

  young :: Ord a => [a] -> a
  young xs = head (sort xs)

  mySort :: [Char] -> [Char]
  mySort = L.sort

  -- signifier :: [Char] -> Char
  -- signifier :: Ord a => [a] -> a
  -- signifier xs = head (mySort xs)

  -- ======================================================

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk aToB a b =
    aToB a == b

  arith :: Num b
        => (a -> b)
        -> Integer
        -> a 
        -> b
  arith aToB int a = aToB a