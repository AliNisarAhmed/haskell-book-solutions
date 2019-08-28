module FoldableInstances where

import Data.Monoid
import Data.Foldable

-- data Identity a
--   = Identity a deriving (Eq, Show)

-- instance Foldable Identity where
--   foldr f z (Identity x) = f x z
--   foldl f z (Identity x) = f z x
--   foldMap f (Identity x) = f x

-- data Optional a
--   = J a
--   | N deriving (Eq, Show)

-- instance Foldable Optional where
--   foldr f z (J x) = f x z
--   foldr f z N = z
--   foldl f z (J x) = f z x
--   foldl f z N = z
--   foldMap f (J x) = f x
--   foldMap f N = mempty

-----------------------  EXERCISES -----------------------

sum' ::(Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs
-- sum' xs = foldr (+) 0 xs

product' :: (Foldable t, Num a) => t a -> a
-- product' = foldr (*) 1
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . (== x)) xs

------ my solution ----------------
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs
  | length xs == 0 = Nothing
  | otherwise = (Just . head) $ (foldr reducer [] xs)
    where
      reducer v [] = [v]
      reducer currentVal (minValue:_) =
        if currentVal < minValue
        then [currentVal]
        else [minValue]

-------- Online: using foldr ----------------
minimumA :: (Foldable t, Ord a) => t a -> Maybe a
minimumA = foldr minMaybe Nothing

minMaybe :: Ord a => a -> Maybe a -> Maybe a
minMaybe x (Just y) = Just $ min x y
minMaybe x _ = Just x

---------- Using foldmap -------------

-- we need a datatype with a monoid instance

newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Semigroup (Min a) where
  (<>) (Min Nothing) x = x
  (<>) x (Min Nothing) = x
  (<>) (Min x) (Min y) = Min $ (min x y)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

myMinimum1 :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum1 xs = getMin $ foldMap (Min . Just) xs


------------- Maximum : using foldr ----------------------

maximum1 :: (Foldable t, Ord a) => t a -> Maybe a
maximum1 = foldr maxMaybe Nothing

maxMaybe :: Ord a => a -> Maybe a -> Maybe a
maxMaybe x (Just y) = Just $ max x y
maxMaybe x _ = Just x

------ using foldmap ------

newtype Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Semigroup (Max a) where
  (<>) (Max Nothing) x = x
  (<>) x (Max Nothing) = x
  (<>) (Max x) (Max y) = Max (max x y)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

maximum2 :: (Foldable t, Ord a) => t a -> Maybe a
maximum2 xs = getMax $ foldMap (Max . Just) xs

---------------------------------------------------------

null' :: (Foldable t) => t a -> Bool
null' xs = foldr reducer True xs
  where
    reducer _ True = False
    reducer _ False = False

  -- or can be simplified
  -- null' = foldr (\_ _ -> False) True
  -- if the reducer runs even for once, we know the answer should be False

x = null' [] -- True
y = null' [1, 2, 3] -- False
a = null' Nothing -- True
b = null' (Just 2) -- False

-------------------------------------------

length' :: (Foldable t) => t a -> Int
length' = foldr (\x acc -> acc + 1) 0

lengthF :: (Foldable t) => t a -> Int
lengthF xs = getSum $ foldMap (\x -> Sum 1) xs

--------------------------------------------

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

toListF :: (Foldable t) => t a -> [a]
toListF = foldMap (\x -> [x])

-------------------------------------

fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap id xs

fold'' xs = foldr (<>) mempty xs

--------------------------------------

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x acc -> f x <> acc) mempty xs

-------------------------------------------------------------


-- Writing Foldable t instances

data Constant a b
  = Constant b deriving (Eq, Show)

instance (Semigroup b) => Semigroup (Constant a b) where
  (<>) (Constant b1) (Constant b2) = Constant (b1 <> b2)

instance (Monoid b) => Monoid (Constant a b) where
  mempty = Constant mempty

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b

-------------------------------------------

data Two a b
  = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

----------------------

data Three a b c
  = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-------------------------

data Threee a b
  = Threee a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Threee a b) where
  (<>) (Threee a1 b1 c1) (Threee a2 b2 c2) =
    Threee (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a, Monoid b) => Monoid (Threee a b) where
  mempty = Threee mempty mempty mempty

-- Monoid and Semigroup instances are not needed

instance Foldable (Threee a) where
  foldMap f (Threee a b1 b2) = (f b1) <> (f b2)

---------------------------------------------------------

data Four a b
  = Four a b b b deriving (Eq, Show)

instance Foldable (Four a) where
  foldMap f (Four a b1 b2 b3) = (f b1) <> (f b2) <> (f b3)

-----------------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f xs = foldMap mapper xs
  where
    mapper x =
      if f x
      then pure x
      else mempty

--------------------------------------------------------