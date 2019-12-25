module Monoid5 where

  import Test.QuickCheck

  newtype Mem s a
    = Mem {
      runMem :: s -> (a, s)
    }

  combineMems f g x = 
    let 
      (a, b) = g x
      (c, d) = f b
    in
      (a <> c, d)

  instance (Semigroup a) => Semigroup (Mem s a) where
    (Mem {runMem = f}) <> (Mem {runMem = g}) = Mem $ combineMems f g

  instance (Monoid a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

  f' = Mem $ \s -> ("hi", s + 1)

  main = do 
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0 