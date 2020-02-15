{-# LANGUAGE FlexibleInstances #-}

module Practice where 

import Control.Monad
import Control.Applicative
import Data.Monoid

-------------------------------------------------------

-- Traversable Instances

-- newtype Identity a 
--   = Identity a 
--   deriving (Eq, Show, Ord)

instance Foldable Identity where
  foldMap f (Identity a) = f a 
  foldr f z (Identity a) = f a z

instance Traversable Identity where 
  traverse f (Identity a) = fmap Identity $ f a


--- CONSTANT --

newtype Constant a b 
  = Constant { getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant a) where 
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where 
  foldMap f (Constant a) = mempty
  foldr f z (Constant a) = z

instance Traversable (Constant a) where 
  traverse f (Constant a) = pure $ Constant a

-- MAYBE -- 

data Optional a 
  = Nada 
  | Yep a 
  deriving (Eq, Show)

instance Functor Optional where 
  fmap _ Nada = Nada 
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where 
  foldMap f (Nada) = mempty
  foldMap f (Yep a) = f a
  foldr f z (Nada) = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where 
  traverse f Nada = pure Nada 
  traverse f (Yep a) = Yep <$> f a

-- List -- 

instance Foldable List where 
  foldMap _ Nil = mempty
  foldMap f (Cons v list) = f v <> foldMap f list
  foldr f z Nil = z 
  foldr f z (Cons v list) = f v (foldr f z list) 

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons v list) = 
    Cons <$> f v <*> traverse f list

-- THREE --

data Three a b c 
  = Three a b c 
  deriving (Eq, Show, Ord)

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where 
  foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where 
  traverse f (Three a b c) = 
    Three a b <$> f c

-- PAIR --

data Pair a b 
  = Pair a b

instance Functor (Pair a) where 
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where 
  foldMap f (Pair a b) = f b
  foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where 
  traverse f (Pair a b) = 
    Pair a <$> f b

-- BIG --

data Big a b 
  = Big a b b
  deriving (Eq, Show)

-- instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where 
--   Big a1 b1 b2 <> Big a2 b3 b4 = 
--     Big (a1 <> a2) (b1 <> b3) (b2 <> b4)

-- instance (Monoid a, Monoid b) => Monoid (Big a b) where 
--   mempty = Big mempty mempty mempty
--   mappend = (<>)

instance Functor (Big a) where 
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where 
  foldMap f (Big a b1 b2) = f b1 <> f b2
  foldr f z (Big a b1 b2) = 
    f b1 (f b2 z)

instance Traversable (Big a) where 
  traverse f (Big a b1 b2) = 
    Big a <$> f b1 <*> f b2

---- BIGGER ----------

data Bigger a b 
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where 
  fmap f (Bigger a b1 b2 b3) = 
    Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where 
  foldMap f (Bigger a b1 b2 b3) = 
    f b1 <> f b2 <> f b3
  foldr f z (Bigger a b1 b2 b3) = 
    f b1 (f b2 (f b3 z))

instance Traversable (Bigger a) where 
  traverse f (Bigger a b1 b2 b3) = 
    Bigger a <$> f b1 <*> f b2 <*> f b3

----------- S ----------------

data S n a 
  = S (n a) a 
  deriving (Eq, Show)

instance Functor n => Functor (S n) where 
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where 
  foldMap f (S na a) = foldMap f na <> f a
  foldr f z (S na a) = 
    f a (foldr f z na)

instance Traversable n => Traversable (S n) where 
  traverse f (S na a) = 
    S <$> (traverse f na) <*> f a

----- TREE ----

data Tree a 
  = Empty 
  | Leaf a 
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where 
  fmap f Empty = Empty 
  fmap f (Leaf a) = Leaf $ f a 
  fmap f (Node left v right) = 
    Node (fmap f left) (f v) (fmap f right)

instance Foldable Tree where 
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a 
  foldMap f (Node left v right) = 
    foldMap f left <> f v <> foldMap f right

instance Traversable Tree where 
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a 
  traverse f (Node left v right) = 
    Node <$> traverse f left <*> f v <*> traverse f right

-------------------------------------------------------

-- Foldable Instances 

-- data Constant a b
--   = Constant b deriving (Show)

-- instance Foldable (Constant a) where 
--   foldMap f (Constant a) = f a
--   foldr f z (Constant a) = f a z

data Two a b 
  = Two a b deriving (Show)

instance Foldable (Two a) where 
  foldMap f (Two a b) = f b
  foldr f z (Two a b) = f b z

-- data Three a b 
--   = Three a b b

-- instance Foldable (Three a) where 
--   foldMap f (Three a b1 b2) = f b1 <> f b2


-- filterF :: (Applicative f, Foldable t, Monoid (f a))
--         => (a -> Bool) -> t a -> f a
-- filterF f ta = foldMap Any $ filter f ta





----------------------------------------------------

mySum :: (Foldable t, Num a) => t a -> a
-- mySum ta = getSum $ foldMap Sum ta
mySum ta = foldr (+) 0 ta

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = getProduct . foldMap Product

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x tx = foldr (\y acc -> y == x) False tx

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum ta = foldr minMaybe Nothing ta 
  where 
    minMaybe x (Just y) = Just $ min x y
    minMaybe x _ = Just x

mNull :: (Foldable t) => t a -> Bool
mNull ta = foldr (\_ _ -> False) True ta

mLength :: (Foldable t) => t a -> Int 
mLength = getSum . foldMap (const $ Sum 1)

mToList :: (Foldable t) => t a -> [a]
mToList = foldr (:) [] 

mFold :: (Foldable t, Monoid m) => t m -> m 
mFold = foldMap id

mFoldMap :: (Foldable t, Monoid m)
         => (a -> m) -> t a -> m 
mFoldMap f ta = foldr (\x acc -> f x <> acc) mempty ta

--------------------------------------------------------

j :: Monad m => m (m a) -> m a
j mm = mm >>= id 

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh list f = traverse f list  
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f 

flipType :: Monad m => [m a] -> m [a]
flipType list = meh list id

--------------------------------------------------

data Nope a = Nope deriving Show

instance Functor Nope where 
  fmap _ Nope = Nope

instance Applicative Nope where 
  pure _ = Nope 
  (<*>) _ _ = Nope

instance Monad Nope where 
  return = pure 
  Nope >>= _ = Nope


data Bah b a
  = PLeft a 
  | PRight b deriving (Eq, Show)

instance Functor (Bah b) where 
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Monoid b => Applicative (Bah b) where 
  pure x = PLeft x
  (<*>) (PRight b) (PRight c) = PRight (b <> c)
  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft f) (PLeft a) = PLeft $ f a

instance Monoid b => Monad (Bah b) where 
  return = pure 
  (PRight b) >>= _ = (PRight b)
  (PLeft a) >>= f = f a




--------------------------------------------------

-- data Sum a b 
--   = First a 
--   | Second b 
--   deriving (Eq, Show)

-- instance Functor (Sum a) where 
--   fmap _ (First a) = First a
--   fmap f (Second b) = Second $ f b

-- instance Monoid a => Applicative (Sum a) where 
--   pure = Second 
--   (<*>) (First a1) (First a2) = First (a1 <> a2)
--   (<*>) (First a) _ = First a
--   (<*>) _ (First a) = First a 
--   (<*>) (Second f) (Second x) = Second $ f x

-- instance Monoid a => Monad (Sum a) where 
--   return = pure 
--   (First a) >>= _ = First a
--   (Second b) >>= f = f b 

sayHi :: String -> IO String 
sayHi greeting = do 
  putStrLn greeting 
  getLine 

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int 
getAge = sayHi >=> readM

askForAge :: IO Int 
askForAge = getAge "Hello, how old are you"


----------------------------------------------

-- data Pair a = Pair a a deriving Show

-- instance Functor Pair where 
--   fmap f (Pair a b) = Pair (f a) (f b)

-- instance Applicative Pair where 
--   pure x = Pair x x
--   (<*>) (Pair f1 f2) (Pair x1 x2) = 
--     Pair (f1 x1) (f2 x2) 


-- data Three a b = Three a b b 

-- instance Functor (Three a) where 
--   fmap f (Three a b1 b2 ) = Three a (f b1) (f b2)

-- instance Monoid a => Applicative (Three a) where
--   pure x = Three mempty x x 
--   (<*>) (Three a1 f1 f2) (Three a2 x1 x2) = 
--     Three (a1 <> a2) (f1 x2) (f2 x2)

stops = "pbtdkg"

vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c = liftA3 (,,) a b c

-------------------------------------------------

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where 
  fmap f Nil = Nil
  fmap f (Cons v rest) = Cons (f v) (fmap f rest)

instance Applicative List where 
  pure x = Cons x Nil
  (<*>) Nil x = Nil
  (<*>) x Nil = Nil
  (<*>) (Cons f fs) ys = 
    fmap f ys `append` (<*>) fs ys

instance Monad List where 
  return = pure
  (Cons a rest) >>= f = f a `append` (rest >>= f)

append :: List a -> List a -> List a 
append Nil ys = ys 
append (Cons x xs) ys = 
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b 
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = concat' $ fmap f as


--------------------------------------------------------

check = const <$> Just "Hello" <*> Just "World"

check2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

newtype Const a b 
  = Const { getConst :: a }


-- instance Functor (Const a) where 
--   fmap f (Const a) = Const a 

-- instance Monoid a => Applicative (Const a) where 
--   pure x = Const mempty
--   (<*>) (Const a) (Const b) = Const (a <> b)

------------------------------------------------------------


newtype Identity a
  = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where 
  pure = Identity 
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- APPLICATIVE EXERCISES

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys 

y :: Maybe Integer 
y = lookup 2 $ zip xs ys 

summed :: Maybe Integer 
summed =  fmap sum ( (,) <$> x <*> y) 

-- FUNCTOR EXERCISES

data Parappa f g a 
  = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b 
  = IgnoreSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

data Notorious g o a t 
  = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = 
      Notorious go ga (fmap f gt)

-- data List a 
--   = Nil 
--   | Cons a (List a)

-- instance Functor List where 
--   fmap _ Nil = Nil 
--   fmap f (Cons v list) = Cons (f v) $ fmap f list

data GoatLord a 
  = NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3)
    = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a 
  = Halt 
  | Print String a 
  | Read { getFunc :: String -> a }

instance Functor TalkToMe where 
    fmap f Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read sa) = Read $ fmap f sa 

f = read :: String -> Int
-- x = Read read :: TalkToMe Int