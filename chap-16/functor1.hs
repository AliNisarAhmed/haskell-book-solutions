module Functor1 where

  data FixMePls a
    = FixMe
    | Pls a deriving (Eq, Show)

  instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

  -- !! breaking Identity Law

  data WhoCares a
    = A
    | B a 
    | C deriving (Eq, Show)

  instance Functor WhoCares where
    fmap _ A = C  -- here breaks the functor id law
    fmap _ C = A  -- here as well
    fmap f (B a) = B (f a)

  -- hence => fmap id A !== id A -> thus breaking the id law

  -- !! breaking the composition law
  -- fmap (f . g) == fmap f . fmap g

  data CountingBad a
    = H Int a deriving (Eq, Show)

  instance Functor CountingBad where
    fmap f (H n a) = H (n+1) (f a)

  -- to see how it breaks the above law.

  oneWhoKnocks = H 0 "Uncle"

  j = (++ " Jesse")
  k = (++ " lol")

  x = fmap (j . k) oneWhoKnocks
  y = fmap j . fmap k $ oneWhoKnocks

  -- x !== y -> Hence breaking the law

  -- To fix, we must not touch any type which is not the last type in kind signature of WhoCares
  