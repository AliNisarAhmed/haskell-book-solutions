module Optional where

  -- Semigtroup is now a super class of Monoid
  -- hence we need a Supergroup instance of Optional
  -- https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

  data Optional a
    = Nada
    | Only a deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Nada

  instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only x) = Only x
    (Only x) <> Nada = Only x
    (Only x) <> (Only y) = Only (x <> y)
    Nada <> Nada = Nada
  
  