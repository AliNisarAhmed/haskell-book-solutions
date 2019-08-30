module Tuple where

instance Functor ( (,) a ) where
  fmap f (x, y) = (x, f y)

instance Monoid a => Applicative ( (,) a) where
  pure = (,) mempty
  (a, f) <*> (x, y) = (a <> x, f y)

instance Foldable ( (,) a ) where
  foldMap f (x, y) = f y
  foldr f z (x, y) = f y z

instance Traversable ( (,) a) where
  traverse f (x, y) = (,) x <$> f y