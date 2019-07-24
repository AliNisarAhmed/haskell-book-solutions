module Lib where

  import Control.Monad
  import Data.Monoid
  import Test.QuickCheck
  import Data.Monoid (Sum, Product)

  -- Important

  -- https://stackoverflow.com/questions/44818972/what-is-the-mempty-of-the-data-type/44821801

  data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

  instance (Semigroup a) => Semigroup (Optional a) where
    (<>) Nada x = x
    (<>) y Nada = y
    (<>) (Only a) (Only b) = Only (a <> b)

  instance (Monoid a) => Monoid (Optional a) where
    mempty = Nada


  -- --------------------

  monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
  monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

  monoidLeftId :: (Eq m, Monoid m) => m -> Bool
  monoidLeftId a = (mempty <> a) == a

  monoidRightId :: (Eq a, Monoid a) => a -> Bool
  monoidRightId a = (a <> mempty) == a

  data Bull
    = Fools
    | Twoo
    deriving (Eq, Show)

  instance Arbitrary Bull where
    arbitrary =
      frequency [ (1, return Fools)
                , (1, return Twoo) ]

  instance Semigroup Bull where
    (<>) _ _ = Fools

  instance Monoid Bull where
    mempty = Fools

  type BullMappend =
    Bull -> Bull -> Bull -> Bool

  -- main :: IO ()
  -- main = do
  --   let
  --     ma = monoidAssoc
  --     mli = monoidLeftId
  --     mri = monoidRightId
  --   quickCheck (ma :: BullMappend)
  --   quickCheck (mli :: Bull -> Bool)
  --   quickCheck (mri :: Bull -> Bool)

  -- ----------------------

  -- newtype First' a
  --   = First' { getFirst :: Optional a }
  --   deriving (Eq, Show)

  -- instance Semigroup a => Semigroup (First' a) where
  --   (<>) (First' opt1) (First' opt2) = First' (opt1 <> opt2)

  -- instance Semigroup a => Monoid (First' a) where
  --   mempty = First' Nada

  -- instance Arbitrary a => Arbitrary (First' a) where
  --   arbitrary = do
  --     a <- arbitrary
  --     frequency [ (1, return $ First' Nada)
  --               , (1, return $ First' (Only a))]

  -- firstMappend :: Semigroup a => First' a -> First' a -> First' a
  -- firstMappend = mappend

  -- type FirstMappend
  --   = First String -> First String -> First String -> Bool

  -- type FstId = First String -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (monoidAssoc :: FirstMappend)
  --   quickCheck (monoidLeftId :: FstId)
  --   quickCheck (monoidRightId :: FstId)

  ----------------------------------------------------------

  data Trivial
    = Trivial deriving (Eq, Show)

  instance Semigroup Trivial where
    _ <> _ = Trivial

  instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

  instance Arbitrary Trivial where
    arbitrary = return Trivial

  semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: TrivialAssoc)
  --   quickCheck (monoidLeftId :: Trivial -> Bool)
  --   quickCheck (monoidRightId :: Trivial -> Bool)

  ----------------------------------------------------------------

  newtype Identity a
      = Identity a deriving (Eq, Show)

  instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

  instance Monoid a => Monoid (Identity a) where
    mempty = Identity (mempty)
    mappend = (<>)

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return (Identity a)

  type IdentityAssoc =
    Identity String -> Identity String -> Identity String -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: TrivialAssoc)
  --   quickCheck (monoidLeftId :: Identity String -> Bool)
  --   quickCheck (monoidRightId :: Identity String -> Bool)

  ---------------------------------------------------------------------

  data Two a b
    = Two a b deriving (Eq, Show)

  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

  instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

  type TwoAssoc =
       Two String String
    -> Two String String
    -> Two String String
    -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: TwoAssoc)
  --   quickCheck (monoidLeftId :: Two String String -> Bool)
  --   quickCheck (monoidRightId :: Two String String -> Bool)

  -----------------------------------------------------------------

  data Three a b c
    = Three a b c deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Three a b c) where
      (Three x1 y1 z1) <> (Three x2 y2 z2) =
        Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

  instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
      arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

  type ThreeAssoc =
       Three String (Sum Int) (Product Int)
    -> Three String (Sum Int) (Product Int)
    -> Three String (Sum Int) (Product Int)
    -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: ThreeAssoc)
  --   quickCheck (monoidLeftId :: Three String (Sum Int) (Product Int) -> Bool)
  --   quickCheck (monoidRightId :: Three String (Sum Int) (Product Int) -> Bool)

  -----------------------------------------------------------

  newtype BoolConj
    = BoolConj Bool deriving (Eq, Show)

  instance Semigroup BoolConj where
    (BoolConj False) <> _               = BoolConj False
    _ <> (BoolConj False)               = BoolConj False
    (BoolConj True)  <> (BoolConj True) = BoolConj True

  instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

  instance Arbitrary BoolConj where
    arbitrary = do
      a <- arbitrary
      return $ BoolConj a

  type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: BoolConjAssoc)
  --   quickCheck (monoidLeftId :: BoolConj -> Bool)
  --   quickCheck (monoidRightId :: BoolConj -> Bool)

-------------------------------------------------------------------

  newtype BoolDisj
      = BoolDisj Bool deriving (Eq, Show)

  instance Semigroup BoolDisj where
    (BoolDisj True) <> _                 = BoolDisj True
    _ <> (BoolDisj True)                 = BoolDisj True
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False

  instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

  instance Arbitrary BoolDisj where
    arbitrary = do
      a <- arbitrary
      return $ BoolDisj a

  type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: BoolDisjAssoc)
  --   quickCheck (monoidLeftId :: BoolDisj -> Bool)
  --   quickCheck (monoidRightId :: BoolDisj -> Bool)

  -----------------------------------------------------------------

  data Or a b
    = Fst a
    | Snd b deriving (Eq, Show)

  instance Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    _ <> (Snd x) = Snd x
    (Fst x) <> (Fst y) = Fst y  -- coz of this, MONOID law will always fail

  instance Monoid a => Monoid (Or a b) where
    mempty = Fst mempty
    mappend = (<>)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return $ Fst a), (1, return $ Snd b)]

  type OrAssoc =
    Or String String -> Or String String -> Or String String -> Bool

  -- main :: IO ()
  -- main = do
  --   quickCheck (semigroupAssoc :: OrAssoc)
  --   quickCheck (monoidLeftId :: Or String String -> Bool)
  --   quickCheck (monoidRightId :: Or String String -> Bool)

  ---------------------------------------------------------------

  newtype Combine a b
    = Combine { unCombine :: (a -> b) }

  -- instance (Eq a, Eq b) => Eq (Combine a b) where
  --   (==) (Combine f) (Combine g) = f == g

  instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (f <> g)

  instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (const mempty)  -- a function that takes in a and returns b, always
    mappend = (<>)

  -- instance (CoArbitrary a, CoArbitrary b, Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  --   arbitrary = do
  --     x <- (arbitrary :: Gen (a -> b))
  --     return $ Combine x

  -- type CombineAssoc =
  --      Combine (Sum Int) (Sum Int)
  --   -> Combine (Sum Int) (Sum Int)
  --   -> Combine (Sum Int) (Sum Int)
  --   -> Bool

  -- main :: IO ()
  -- main =
  --   quickCheck (semigroupAssoc :: CombineAssoc)

  ---------------------

  newtype Comp a
    = Comp { unComp :: (a -> a) }

  instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f <> g)

  instance Monoid a => Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

  -----------------------------------

  -- data Validation a b
  --   = Failure a
  --   | Success b deriving (Eq, Show)

  -- instance (Semigroup a) => Semigroup (Validation a b) where
  --   (Success x) <> _ = Success x
  --   _ <> (Success x) = Success x
  --   (Failure x) <> (Failure y) = Failure (x <> y)

  -- main = do
  --   let failure :: String -> Validation String Int
  --       failure = Failure
  --       success :: Int -> Validation String Int
  --       success = Success
  --   print $ success 1 <> failure "blah"
  --   print $ failure "woot" <> failure "blah"
  --   print $ success 1 <> success 2
  --   print $ failure "woot" <> success 2


  ----------------------------------


  newtype Mem s a
    = Mem {
      runMem :: s -> (a, s)
    }

  instance Semigroup a => Semigroup (Mem s a) where
    (Mem x) <> (Mem y) = Mem ()
      where
        rm1 = runMem (Mem x)
        rm2 = runMem (Mem y)