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

-- instance Functor m => Functor (StateT s m) where 
--   fmap f (StateT sma) = StateT (\s -> 
--       let 
--         mas = sma s
--         x = fmap f mas
--       in
--         _a f mas
--     )
  -- sma :: s -> m (a, s)
  -- f :: a -> b