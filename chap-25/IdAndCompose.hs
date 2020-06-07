{-# LANGUAGE InstanceSigs #-}

module IdAndCompose where

-- import Control.Applicative

newtype Identity a
  = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

newtype Compose f g a
  = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure

  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- foldMap => (a -> m) -> t1 a -> m
-- foldMap . foldMap => (a -> m) -> t1 (t2 a) -> m
-- so what (foldMap . foldMap) does is that it pierces the two foldable structures
  -- applies the function f => (a -> m) and returns the monoid m
-- so in essense, the composition of foldMaps = (foldMap . foldMap) gives us a foldMap for two structures


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- f :: a -> f1 b
-- fga :: f (g a)
-- travserse . traverse :: (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b)
-- traverse . traverse :: (a -> f1 b) -> f (g a) -> f1 (f (g b))
-- since the output of the traverse instance requires f1 (Compose f (g b)), we fmap it over f1 

class Bifunctor p where 
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c 
        -> p b d
  bimap f g = first f . second g

first :: Bifunctor p => (a -> c) -> p a b -> p c b
first f = bimap f id

second :: Bifunctor p => (b -> c) -> p a b -> p a c
second = bimap id

-- Bifunctor instances 

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where 
  bimap f g (Deux a b) = Deux (f a) (g b)


-------------------

data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where 
  bimap f g (Const a) = Const (f a)

-----------------------

data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where 
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-------------------------

data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where 
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

------------------------


data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where 
  bimap f g (SemiDrei a) = SemiDrei a

------------------------


data Quad a b c d = Quad a b c d

instance Bifunctor (Quad a b) where 
  bimap f g (Quad a b c d) = Quad a b (f c) (g d)

-------------------------


data E a b = L a | R b deriving (Eq, Show)

instance Bifunctor E where 
  bimap f g (L a) = L (f a)
  bimap f g (R b) = R (g b)