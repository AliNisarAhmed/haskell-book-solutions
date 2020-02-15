{-# LANGUAGE InstanceSigs #-}

module Revision where 

newtype Reader r a 
  = Reader { runReader :: r -> a }

instance Functor (Reader r) where 
  fmap f (Reader ra) = Reader $ f . ra

-- instance Applicative (Reader r) where 
--   pure x = Reader $ const x
--   (<*>) (Reader rf) (Reader ra) = 

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