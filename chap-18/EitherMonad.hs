module EitherMonad where

type Founded = Int

type Coders = Int

data SoftwareShop
  = Shop {
    founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $  TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Founded
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware yrs c = do
  years <- validateFounded yrs
  coders <- validateCoders c
  if coders > div years 10
    then Left $ TooManyCodersForYears years coders
    else Right $ Shop years coders


------------------- Exer ---------------------

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Monoid a => Applicative (Sum a) where
  pure = Second
  (<*>) (First a1) (First a2) = First (a1 <> a2)
  (<*>) (First a) _ = First a
  (<*>) _ (First b) = First b
  (<*>) (Second f) (Second x) = Second (f x)

instance Monoid a => Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     frequency [(1, return $ First x), (1, return $ Second y)]

-- instance (Eq a, Eq b) => EqProp (Sum a b) where
--   (=-=) = eq

-- type RightMonad = Sum String (Int, String, Int)

-- main :: IO ()
-- main = quickBatch $ monad (Second (1, "ali", 2) :: RightMonad)