module AllAboutMonads where

import Control.Monad (mplus, MonadPlus(..))
import Data.Maybe (maybeToList)

data Sheep = Sheep deriving (Eq, Show)

father :: Sheep -> Maybe Sheep
father = undefined

mother :: Sheep -> Maybe Sheep
mother = undefined

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s  = do
  m <- mother s
  father m

maternalGrandfather2 :: Sheep -> Maybe Sheep
maternalGrandfather2 s = mother s >>= father

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do
  m <- mother s
  gf <- father m
  father gf

mothersPaternalGrandfather2 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather2 s = mother s >>= father >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do
  f <- father s
  gm <- mother f
  mother gm

fathersMaternalGrandmother2 :: Sheep -> Maybe Sheep
fathersMaternalGrandmother2 s = father s >>= mother >>= mother

---

parent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` father s

-- mplus returns the first monad that does not fail, else returns fail

grandparent :: Sheep -> Maybe Sheep
grandparent s =
  (mother s >>= father) `mplus`
  (father s >>= mother) `mplus`
  (mother s >>= mother) `mplus`
  (father s >>= father)

---

-- parent2 :: Sheep -> [Sheep]
-- parent2 s = maybeToList (mother s >>= father)

-- grandparent2 :: Sheep -> [Sheep]
-- grandparent2 s = maybeToList $ grandparent s

-- above solution will select either the mother or the father, but not  both

-- to get a list of all the parents and grandparents that meet the criteria, we use mplus

parent2 :: Sheep -> [Sheep]
parent2 s = maybeToList (mother s) ++ maybeToList (father s)

grandparent2 :: Sheep -> [Sheep]
grandparent2 s = 
  maybeToList (mother s >>= father) `mplus`
  maybeToList (father s >>= mother) `mplus`
  maybeToList (father s >>= father) `mplus`
  maybeToList (mother s >>= mother) 

--- 

toMonad :: MonadPlus m => Maybe a -> m a
toMonad Nothing = mzero 
toMonad (Just x) = return x

parent3 :: MonadPlus m => Sheep -> m Sheep
parent3 s = toMonad $ father s `mplus` mother s

grandparent3 :: MonadPlus m => Sheep -> m Sheep
grandparent3 s = 
  toMonad (mother s >>= father) `mplus`
  toMonad (father s >>= mother) `mplus`
  toMonad (father s >>= father) `mplus`
  toMonad (mother s >>= mother) 