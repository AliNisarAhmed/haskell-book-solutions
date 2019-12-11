{-# LANGUAGE DeriveFunctor #-}

module Covariance where

  -- https://www.fpcomplete.com/blog/2016/11/covariance-contravariance

  import Data.Functor.Contravariant
  import Data.Bifunctor
  -- import Data.Profunctor

  newtype MakeString a = MakeString { makeString :: a -> String }

  showInt :: MakeString Int
  showInt = MakeString show

  -- plus3ShowInt :: MakeString Int
  -- plus3ShowInt = MakeString (show . (+3))
  -- the above is not very composable

  -- instance Functor MakeString where
  --   fmap f (MakeString a) = MakeString (a . f)
  -- we can't make Functor instance of MakeString

  instance Contravariant MakeString where
    contramap f (MakeString g) = MakeString (g . f)

  mapMakeString :: (b -> a) -> MakeString a -> MakeString b
  mapMakeString f (MakeString g) = MakeString (g . f)

  -- plus3ShowInt :: MakeString Int
  -- plus3ShowInt = mapMakeString (+3) showInt

  plus3ShowInt :: MakeString Int
  plus3ShowInt = contramap (+3) showInt

  -- main :: IO ()
  -- main = putStrLn $ makeString plus3ShowInt 5

  -----------------------------------------------------------

  greaterThanThree :: Predicate Int
  greaterThanThree = Predicate (> 3)

  lengthGTThree :: Predicate [a]
  lengthGTThree = contramap length greaterThanThree

  englishGTThree :: Predicate Int
  englishGTThree = contramap english lengthGTThree

  english :: Int -> String
  english 1 = "one"
  english 2 = "two"
  english 3 = "three"
  english 4 = "four"
  english 5 = "five"
  english 6 = "six"
  english 7 = "seven"
  english 8 = "eight"
  english 9 = "nine"
  english 10 = "ten"

  main :: IO ()
  main = print $ filter (getPredicate englishGTThree) [1..10]

  -- is Either a Bifunctor or Profunctor

  -- class BiFunctor p where
  --   bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

  class Profunctor p where
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d

  e :: Either Int String
  e = Right "2"

  f :: Either Int String
  f = Left 2

  fLeft :: String -> Int
  fLeft = read

  fRight :: Int -> String
  fRight = show

  -- bimap fRight fLeft e => Just 2
  -- bimap (Int -> String) (String -> Int) (Either Int String)
  -- Either is a Bifunctor

  t1 :: (Int, String)
  t1 = (2 , "2")

  -- bimap fRight fLeft t1 => ("2", 2)
  -- (,) is a BiFunctor

  instance Profunctor (->) where
    dimap f g h = g . h. f

  g :: Int -> String
  g = show

  -- (->) is a Profunctor

  -----------------------------------------------------

  data WithInt a = WithInt (Int -> a)
  data MakeInt a = MakeInt (a -> Int)

  instance Functor WithInt where
    fmap f (WithInt g) = WithInt (f . g)


  intToMaybe :: Int -> Maybe Int
  intToMaybe = Just

  d :: WithInt (Maybe Int)
  d = WithInt intToMaybe

  foo :: Maybe Int -> String
  foo (Just i) = show i

  -- fmap foo d => WithInt String
  -- fmap (Maybe Int -> String) (WithInt Maybe Int) => WithInt String
  -- fmap (a -> c) (WithInt a) => WithInt c
  -- P (a -> c . b -> a)
  -- P c

  instance Contravariant MakeInt where
    contramap f (MakeInt g) = MakeInt (g . f)


  maybeToInt :: Maybe Int -> Int
  maybeToInt (Just x) = x

  h :: MakeInt (Maybe Int)
  h = MakeInt maybeToInt

  bar :: String -> Maybe Int
  bar = Just . read

  -- contramap bar h => MakeInt String
  -- contramap (String -> Maybe Int) (MakeInt (Maybe Int)) => MakeInt String
  -- contramap (a -> b) (MakeInt b) => MakeInt a

  -------------------------------------------------------------------

  -- data MakeInt2 a = MakeInt2 (a -> Int) deriving (Functor)
  -- the above results in ERROR: tells us why MakeInt cant be a functor
  -- because a appears as "an input to a function"


  -- Positive position: the type variable is the result/output/range/codomain of the function
  -- Negative position: the type variable is the argument/input/domain of the function

  -- When a type variable appears in positive position, the data type is covariant with that variable.
  -- When the variable appears in negative position, the data type is contravariant with that variable.