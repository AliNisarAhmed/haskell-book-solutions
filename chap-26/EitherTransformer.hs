module EitherTransformer where

newtype EitherT e m a 
  = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where 
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
  

instance Applicative m => Applicative (EitherT e m) where 
  pure = EitherT . pure . pure
  EitherT f <*> EitherT x = EitherT $ (<*>) <$> f <*> x

instance Monad m => Monad (EitherT e m) where 
  return = pure
  EitherT mea >>= f = EitherT $ do 
    ea <- mea
    case ea of 
      Right r -> runEitherT $ f r
      Left l -> return $ Left l

swapEither :: Either e a -> Either a e
swapEither (Left l) = Right l
swapEither (Right r) = Left r

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT (fmap swapEither ema)


eitherT :: Monad m 
        => (e -> m c)
        -> (a -> m c)
        -> EitherT e m a
        -> m c
eitherT f g (EitherT ema) = do 
  ea <- ema
  case ea of 
    Left e -> f e
    Right b -> g b
