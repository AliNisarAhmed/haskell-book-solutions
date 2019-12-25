module QuickCheckinFunctors where

  import Test.QuickCheck

  -- Functor laws
  -- fmap id = id
  -- fmap ( p . q ) = (fmap p) . (fmap q)

  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIdentity f = 
    fmap id f == f

  functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> (f a) -> Bool  
  functorCompose f g x = 
    (fmap g (fmap f x)) == (fmap (g . f) x)

  -- c = functorCompose (+1) (*2)

  -- Implementing Functors

  newtype Identity a 
    = Identity a deriving (Eq, Show)

  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a

  -- -- -----------------------------------------------------------------------

  data Pair a = 
    Pair a a

  instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

  -- -----------------------------------------------------------------------------

  data Two a b = 
    Two a b deriving (Eq, Show)

  instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b
    
  -- ----------------------------------------------------------------------------

  data Three a b c 
    = Three a b c deriving (Eq, Show)

  instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)


  -- -------------------------------------------------------------------------

  data Three' a b = 
    Three' a b b deriving (Eq, Show)

  instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)
  

  -- -------------------------------------------------------------------------

  data Four a b c d 
    = Four a b c d deriving (Eq, Show)

  instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)

  -- --------------------------------------------------------------------------

  data Four' a b = 
    Four' a a a b deriving (Eq, Show)

  instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y (f z)

  -- -------------------------------------------------------------------------

  data Possibly a 
    = LolNope
    | Yeppers a deriving (Eq, Show)

  instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

  -- -------------------------------------------------------------------------

  data Sum a b
    = First a 
    | Second b deriving (Eq, Show)

  instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)