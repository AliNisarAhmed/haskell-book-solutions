{-# LANGUAGE NoImplicitPrelude #-}

module PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a b = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return ::a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Functor ( (->) r ) where
  fmap = (.)

instance Applicative ( (->) r ) where
  pure = const
  (<*>) rab ra = (\r -> (rab r) (ra r))

instance Monad ( (->) r ) where
  return = pure
  ra >>= f = flip f <*> ra
  -- f :: a -> (r -> b)
  -- flip f :: r -> (a -> b)
  -- ra :: r -> a
