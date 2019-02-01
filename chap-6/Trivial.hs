module Trivial where
  data Trivial = 
    Trivial'
  
  instance Eq Trivial where
    Trivial' == Trivial' = True

  data DayOfWeek = 
    Mon | Tue | Wed | Thu | Fri | Sat | Sun

  data Date = 
    Date DayOfWeek Int

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
    (==) (Date weekday dayOfMonth) (Date wk dom) = 
      weekday == wk && dayOfMonth == dom

  data Identity a = 
    Identity a
  
  instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

  data TisAnInteger = 
    TisAn Integer
  instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a' 

  data TwoIntegers =
    Two Integer Integer
  instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

  data StringOrInt
    = TisAnInt Int
    | TisAString String
  instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString b) (TisAString b') = b == b'
    (==) _ _ = False

  data Pair a = 
    Pair a a
  instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == y && x' == y'
    
  data Tuple a b = 
    Tuple a b
  instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

  data Which a
    = ThisOne a
    | ThatOne a
  instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne y) (ThatOne y') = y == y'
    (==) _ _ = False

  data EitherOr a b
    = Hello a
    | GoodBye b
  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (GoodBye y) (GoodBye y') = y == y'
    (==) _ _ = False