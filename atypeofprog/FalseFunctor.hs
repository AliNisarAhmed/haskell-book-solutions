{-# LANGUAGE PolyKinds, RankNTypes #-}

module FalseFunctor where

  data FalseFunctor a b
    = Lefty a
    | Righty b deriving (Eq, Show)


  -- instance Functor (FalseFunctor a) where
  --   fmap f (Lefty x)  = Lefty $ f x
  --   fmap f (Righty y) = Righty $ f y

  data ReverseFunctor a b
    = RLeft b
    | RRight a deriving (Eq, Show)

  instance Functor (ReverseFunctor a) where
    fmap f (RLeft b) = RLeft $ f b
    fmap f (RRight a) = RRight a

  -- the above passes both functor laws, because left type is not being modified
  -- ReverseFunctor is basically the same as Either

  data Flip a b = Flip (Either b a) deriving (Eq, Show)

  instance Functor (Flip a) where
    fmap f (Flip (Right a)) = Flip $ Right a
    fmap f (Flip (Left b))  = Flip $ Left (f b)

  lefty :: Flip String Int
  lefty = Flip (Right "abc")

  rity :: Flip String Int
  rity = Flip (Left 2)

  f1 :: Int -> Int
  f1 = (+7)

  f2 :: Int -> Int
  f2 = (*20)

  l1 = (fmap f1) . (fmap f2) $ lefty
  l2 = fmap (f1 . f2) lefty

  r1 = (fmap f1) . (fmap f2) $ rity
  r2 = fmap (f1 . f2) rity

  unFlip :: Flip a b -> Either b a
  unFlip (Flip x) = x

  -- Flip and unFlip allow us to fmap over both Right and Left in either
  -- they give rise to a function called bimap

  x = Left 20 :: Either Int Int
  y = Right 30 :: Either Int Int

  -- fmap f1 x == Left 20
  -- fmap f1 y == Right 20

  -- if we want to apply f1 to Left 20, we need to use Flip and unFlip

  -- unFlip $ fmap f1 (Flip (Left x)) => Left 27

  bimap1 :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap1 aToC bToD x = unFlip $ fmap aToC (Flip $ fmap bToD x)

  -- bimap : we give it two functions f1 f2, and depending on Either is Left or Right
  --         it will apply f1 or f2 to that Either


  -- we can define bimap for `Pair a b` as well
  data Pair a b = Pair a b deriving (Eq, Show)

  bimapPair :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
  bimapPair f1 f2 (Pair a b) = Pair (f1 a) (f2 b)

  -- we can generalize bimap to a BiFunctor typeclass, which maps two functions
  -- over a type with * -> * -> * kind

  class BiFunctor (f :: * -> * -> *) where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

  instance BiFunctor Either where
    bimap f1 f2 (Right r) = Right (f2 r)
    bimap f1 f2 (Left x) = Left (f1 x)

  instance BiFunctor Pair where
    bimap f1 f2 (Pair a b) = Pair (f1 a) (f2 b)


  -- we can't use bimap to implement the swap function shown below
  -- bimapForSwap :: (a -> b) -> (b -> a) -> Pair a b -> Pair b a
  -- w/o knowing more about a and b, we can't make the functions (a -> b) & (b -> a)

  swap :: forall (a :: *) (b :: *) . Pair a b -> Pair b a
  swap (Pair a b) = Pair b a

  g1 :: Int -> Maybe Int
  g1 x = Just x

  g2 :: Maybe Int -> Int
  g2 (Just x) = x

  -- bimap g1 g2 (Pair 3 (Just 4)) == Pair (Just 4) 3 == swap (Pair 3 (Just 4))

  -- we can do it above because we are using concrete types