module MaybeTransformer where

import Control.Monad

newtype MaybeT m a = 
  MaybeT { runMaybeT :: m (Maybe a) } 

instance Functor m => Functor (MaybeT m) where 
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma 

instance Applicative m => Applicative (MaybeT m) where 
  pure = MaybeT . pure . pure

  MaybeT f <*> MaybeT x = MaybeT ((<*>) <$> f <*> x)

instance Monad m => Monad (MaybeT m) where 
  return = pure 
  MaybeT x >>= f = MaybeT $ do 
    ma <- x
    -- DID NOT KNOW WE CAN DO CASE EXPRESSIONS IN do Blocks
    case ma of 
      Nothing -> return Nothing 
      Just a -> runMaybeT . f $ a

-- f :: a -> MaybeT m b
-- x :: m (Maybe a)
-- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b