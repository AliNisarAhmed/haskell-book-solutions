{-# LANGUAGE InstanceSigs #-}

module IdAndCompose where

import Control.Applicative

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
  pure :: a -> Compose f g a 
  pure x = Compose . pure . pure $ x

  (<*>) :: Compose f g (a -> b)
        -> Compose f g (a)
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ( fmap (<*>) f <*> a)

instance (Foldable f, Foldable g) => 
  Foldable (Compose f g) where 
    foldMap f (Compose fga) = foldMap f (foldMap f fga) 