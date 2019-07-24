{-# LANGUAGE FlexibleInstances #-}

module Main where

  import Test.QuickCheck

  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIdentity f = fmap id f == f

  functorCompose :: (Functor f, Eq (f c))
                 => (a -> b)
                 -> (b -> c)
                 -> f a
                 -> Bool
  functorCompose f g x = ((fmap g . fmap f) $ x) == (fmap (g . f) x)

  -- f :: [Int] -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: [Int])

  -- main :: IO ()
  -- main = do
  --   quickCheck li

  -------------------------------------

  newtype Identity a
    = Identity a deriving (Eq, Show)

  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

  instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a

  -- f :: Identity Int -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Identity Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  -----------------------------------------------------
  data Pair a =
    Pair a a deriving (Eq, Show)

  instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

  instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Pair a b

  -- f :: Pair Int -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Pair Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  --------------------------------------------------------

  data Two a b
    = Two a b deriving (Eq, Show)

  instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

  -- f :: Two Int String -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Two String Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  ---------------------------------------------------

  data Three a b c
    = Three a b c deriving (Eq, Show)

  instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

  -- f :: Three (Maybe Int) Int String -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Three Int (Maybe Int) Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  -----------------------------------------------------------

  data Th a b
    = Th a b b deriving (Eq, Show)

  instance Functor (Th a) where
    fmap f (Th a b c) = Th a (f b) (f c)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Th a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Th a b b


  -- f :: Th Int String -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Th String Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  --------------------------------------------------------------------

  data Four a b
    = Four a a a b deriving (Eq, Show)

  instance Functor (Four a) where
    fmap f (Four a b c d) = Four a b c (f d)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Four a a a b

  -- f :: Four Int String -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Four String Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  ---------------------------------------------------------

  data Possibly a
    = LolNope
    | Yes a deriving (Eq, Show)

  instance Functor (Possibly) where
    fmap f LolNope = LolNope
    fmap f (Yes x) = Yes $ f x

  instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = do
      a <- arbitrary
      oneof [return LolNope, return $ Yes a]

  -- f :: Possibly Int -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Possibly Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li
  -- main = return ()

  ------------------------------------------------------

  -- data Sum a b
  --   = First a
  --   | Second b deriving (Eq, Show)

  -- instance Functor (Sum a) where
  --   fmap _ (First x) = First x
  --   fmap f (Second y) = Second $ f y

  -- instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  --   arbitrary = do
  --     a <- arbitrary
  --     b <- arbitrary
  --     oneof [return $ First a, return $ Second b]

  -- f :: Sum Int String -> Bool
  -- f x = functorIdentity x

  -- c = functorCompose (+1) (*2)
  -- li x = c (x :: Sum String Int)

  -- main :: IO ()
  -- main = do
  --   quickCheck f
  --   quickCheck li

  ----------------------------------------------

  newtype Constant a b
    = Constant { getConstant :: a} deriving (Eq, Show)

  instance Functor (Constant a) where
    fmap _ (Constant v) = Constant v

  --------------------------

  data Wrap f a =
    Wrap (f a) deriving (Eq, Show)

  instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

  ----------------------------------------

  -- type Nat f g = forall a . f a -> f g

  -- maybeToList :: Nat Maybe []
  -- maybeToList Nothing = []
  -- maybeToList (Just a) = [a]


  -------------------------------------

  data B a
    = F a
    | T a deriving (Eq, Show)

  instance Functor B where
    fmap f (F a) = F (f a)
    fmap f (T a) = T (f a)

  instance Arbitrary a => Arbitrary (B a) where
    arbitrary = do
      a <- arbitrary
      oneof [return $ F a, return $ T a]

  f :: B String -> Bool
  f x = functorIdentity x

  c = functorCompose (+1) (*2)
  li x = c (x :: B Int)

  main :: IO ()
  main = do
    quickCheck f
    quickCheck li

  -------

  -- data MyType a b c
  --   = Ali { getAli :: a}
  --   | Sam (b c) deriving (Eq, Show)

  data MyType a b c
    = Ali a
    | Sam c b deriving (Eq, Show)
  -------------------------------------

  data Mu f
    = InF { outF :: f (Mu f) }

  -- instance Functor Mu where
  --   fmap = undefined

  -- fails because Mu has kind (* -> *) -> *

  -- instance Functor f => Functor (Mu f) where
  --   fmap = undefined

  -- fails coz Mu f has kind *

  -------------------------------------------

  data Company a c b
  -- instead of a b c
    = DeepBlue a c
    | Something b deriving (Eq, Show)

  instance Functor (Company a b) where
    fmap f (Something b) = Something $ f b
    fmap f (DeepBlue a c) = DeepBlue a c

  ------------------------------------------------

  data More b a
    = L a b a
    | R b a b deriving (Eq, Show)

  instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

  -----------------------------------------------

  data Quant a b
    = Finance
    | Desk a
    | Bloor b

  instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor (f b)
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x

  ---------------

  data K a b = K a deriving (Eq, Show)

  -- instance Functor (K a) where
  --   fmap _ (K a) = K a

  --------------------------------------------

  newtype Flip f a b
    = Flip (f b a) deriving (Eq, Show)

  instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

  -----------------

  data GoateeConst a b = GoateeConst b deriving (Eq, Show)

  instance Functor (GoateeConst a) where
    fmap f (GoateeConst b) = GoateeConst (f b)

  ------------------------------------------------------

  data LiftItOut f a
    = LiftItOut (f a) deriving (Eq, Show)

  instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut gx) = LiftItOut (fmap f gx)

  -------------------------------------------------------

  data Parappa f g a
    = DaWrappa (f a) (g a) deriving (Eq, Show)

  instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

  ----------------------------------------------------

  data IgnoreOne f g a b
    = IgnoringSomething (f a) (g b) deriving (Eq, Show)

  instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

  ------------------------------------------------------------

  data Notorious g o a t
    = Notorious (g o) (g a) (g t) deriving (Eq, Show)

  instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

  ---------------------------------------------------------------

  data List a
    = Nil
    | Cons a (List a) deriving (Eq, Show)

  instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x list) = Cons (f x) (fmap f list)

  --------------------------------------------------------------

  data GoatLord a
    = NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

  instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

  ------------------------------------------------------------------------

  data TalkToMe a
    = Halt
    | Print String a
    | Read (String -> a)

  instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    -- fmap f (Read g) = Read (f . g)
    fmap f (Read g) = Read (fmap f g)  -- both work!
