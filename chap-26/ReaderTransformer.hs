module ReaderTransformer where

newtype ReaderT r m a
  = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT f <*> ReaderT x = ReaderT ((<*>) <$> f <*> x)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= f = ReaderT (\r -> do
      a <- rma r
      runReaderT (f a) r
    )

-- rma :: r -> m a
-- f   :: a -> ReaderT r m b 


---- STATE Transformer

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT (fmap (applyToFirst f) . sma)
    where
      applyToFirst func (a, s) = (func a, s)

  -- sma :: s -> m (a, s)
  -- f :: a -> b

-- MOnad instance on m is needed because we are threading the state through to the final result
  -- without it, we would be forced to use the initial state
instance Monad m => Applicative (StateT s m) where 
  pure x = StateT (\s -> pure (x, s))
  (StateT mf) <*> (StateT mx) = StateT (\s -> do 
    (f, s1) <- mf s
    (x, s2) <- mx s1
    return (f x, s2)
    )

-- mf :: s -> m (a -> b, s)
-- mx :: s -> m (a, s)

instance Monad m => Monad (StateT s m) where 
  return = pure 
  StateT mx >>= f = StateT (\s -> do 
      (a, s1) <- mx s
      (runStateT $ f a) s
    )

-- f :: a -> StateT s m b
-- mx :: s -> m (a, s)