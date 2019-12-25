module BindAndFmap where

import Control.Monad (join)

-- fmap f xs = xs >>= return . f

-- (>>=)  :: m a -> (a -> m b) -> m b
-- (>>)   :: m a -> m b -> m b
-- return :: a -> m a
-- join   :: m (m a) -> m a
-- fmap :: (a -> b) -> f a -> f b

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

