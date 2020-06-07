module IdentityTransformer where 

data IdentityT f a = IdentityT 
  { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where 
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative f => Applicative (IdentityT f) where 
  pure x = IdentityT (pure x)
  (IdentityT f) <*> (IdentityT x) = IdentityT (f <*> x)
  -- f :: g (a -> b)
  -- x :: g a 

instance Monad f => Monad (IdentityT f) where 
  return = pure 
  (IdentityT fa) >>= f = IdentityT $ fa >>= runIdentityT . f

-- f :: a -> IdentityT f b 
-- fa :: f a 
-- the above instance in words: 
   -- apply f to a to get (IdentityT f b), then runIdentityT to get f b, give it to IdentityT to get IdentityT (f b)