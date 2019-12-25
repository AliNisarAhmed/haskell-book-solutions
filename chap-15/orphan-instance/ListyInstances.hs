module ListyInstances where

  import Data.Monoid
  import Listy

  instance Semigroup (List a) where
    (<>) (Listy l) (Listy l') = 
      Listy $ mappend l l'

  instance Monoid (List a) where
    mempty = Listy []