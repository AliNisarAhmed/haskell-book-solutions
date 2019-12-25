module ConstantFunc where

  newtype Constant a b 
    = Constant { getConstant :: a } deriving (Eq, Show)

  instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x  -- becase b is phantom and has no value witness


  data Wrap f a
    = Wrap (f a) deriving (Eq, Show)

  -- instance Functor (Wrap f) where
  --   fmap f (Wrap m) = Wrap (f m)  -- will not work

  instance Functor f => Functor (Wrap f) where
    fmap f (Wrap m) = Wrap (fmap f m)    -- we have to lift f

  getInt :: IO Int
  getInt = fmap read getLine