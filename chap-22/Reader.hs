-- useless: {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module ReaderType where

newtype Reader r a
  = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap f (Reader ra) = Reader $ fmap f ra
  -- but f is a func, ra is a func, so fmap f ra == f . rs
  fmap f (Reader ra) = Reader $ f . ra


ask :: Reader a a
ask = Reader id

--------------------------------------------------------------

-- Applicative Instance for Reader r a

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra
  -- rab = r -> (a -> b) => f (a -> b)
  -- ra = r -> a => f a
  -- rab <*> ra = f a => r -> b => give it to Reader, we have Reader r b
  -- OR can also be written as
  -- Reader (\r -> (rab r) (ra r))

x :: Reader r (Int -> Maybe Int)
x = Reader (const Just)

y :: Reader r Int
y = Reader (const 2)

z :: Reader r (Maybe Int)
z = x <*> y

------------------------------------------------------------

-- Monad Instance for Reader

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r)

-------------------------------

-- Applicative of a function

newtype HumanName
  = HumanName String
    deriving (Eq, Show)

newtype DogName
  = DogName String
    deriving (Eq, Show)

newtype Address
  = Address String
    deriving (Eq, Show)

data Person
  = Person {
    humanName :: HumanName
  , pDogName :: DogName
  , pAddress :: Address
  } deriving (Eq, Show)

data Dog
  = Dog {
    dDogName :: DogName
  , dAddress :: Address
  } deriving (Eq, Show)

ali :: Person
ali =
  Person (HumanName "ali") (DogName "tommy") (Address "Punchcard")

sam :: Person
sam =
  Person (HumanName "samrah") (DogName "timmy") (Address "CBI")

getDog1 :: Person -> Dog
-- getDog1 p = Dog (dogName (p :: Person)) (address (p :: Person))
getDog1 (Person _ d a) = Dog d a

getDogR :: Person -> Dog
getDogR = Dog <$> pDogName <*> pAddress
-- applies pDogName to Person, pAddress to Person, and combines the results
-- with Dog (a binary function)
-- OR liftA2 Dog pDogName pAddress

----------------------------------------------------------------

myLiftA2 :: Applicative f =>
  (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

--------------------------------------------

