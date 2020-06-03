{-# LANGUAGE InstanceSigs #-}

module Revision where 

-- Exercise solution

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer 
xs = lookup 3 $ zip x y

ys :: Maybe Integer 
ys = lookup 6 $ zip y z

zs :: Maybe Integer 
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer 
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) (z' n)

summed :: Num a => (a, a) -> a
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

-- main :: IO ()
-- main = do 
--   print $ 
--     sequenceA [Just 3, Just 2, Just 1]
--   print $ sequenceA [x, y]
--   print $ sequenceA [xs, ys]
--   print $ summed <$> ((,) <$> xs <*> ys)
--   print $ fmap summed ((,) <$> xs <*> zs)
--   print $ bolt 7
--   print $ fmap bolt z

seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [ (> 3), (< 8), even ] m

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do 
  print $ getAll $ foldMap All $ seqA 12
  print $ seqA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys









-------------------------------------------
newtype Reader r a 
  = Reader { runReader :: r -> a }

instance Functor (Reader r) where 
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a 
  pure x = Reader $ const x

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (<*>) (Reader rf) (Reader ra) =
     Reader $ \r -> 
       (rf r) (ra r)
  -- OR 
  -- (<*>) (Reader rf) (Reader ra) = 
    -- Reader $ rf <*> ra

instance Monad (Reader r) where 
  return = pure 
  (>>=) :: Reader r a 
        -> (a -> Reader r b)
        -> Reader r b
  (>>=) (Reader a) f = 
    Reader $ \r -> runReader (f $ a r) $ r

myLiftA2 :: Applicative f 
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2  f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a 
asks f = Reader f

ask :: Reader a a 
ask = Reader id

newtype HumanName = 
  HumanName String 
  deriving (Eq, Show)

newtype DogName = 
  DogName String 
  deriving (Eq, Show)

newtype Address = 
  Address String 
  deriving (Eq, Show)

data Person = 
  Person {
    humanName :: HumanName
  , dogName :: DogName 
  , address :: Address
  } deriving (Eq, Show)

data Dog
  = Dog {
    dogsName :: DogName
  , dogAddress :: Address
  } deriving (Eq, Show)

p1 :: Person 
p1 = 
  Person (HumanName "Ali")
         (DogName "Tommy")
         (Address "Sesame Street")

p2 :: Person 
p2 = 
  Person (HumanName "Sam") 
         (DogName "Pike")
         (Address "Edmonton")

getDog :: Person -> Dog 
getDog p = 
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do 
  name <- dogName 
  add <- address
  return $ Dog name add