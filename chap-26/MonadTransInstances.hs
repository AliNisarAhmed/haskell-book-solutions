module MonadTransInstances where 

import MaybeTransformer
import EitherTransformer
import Control.Monad (liftM)
import ReaderTransformer

-- class MonadTrans t where 
--   lift :: (Monad m) => m a -> t (m a)

-- instance MonadTrans MaybeT where 
--   lift = MaybeT . liftM Just

-- instance MonadTrans (EitherT e m) where 
--   lift = EitherT . liftM Right

-- instance MonadTrans (StateT s ) where 
--   lift = StateT _a