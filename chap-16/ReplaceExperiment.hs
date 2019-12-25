module ReplaceExperiment where

  replaceWithP :: a -> Char
  replaceWithP = const 'p'

  lms :: [Maybe [Char]]
  lms = [Just "Ave", Nothing, Just "woohoo"]

  -- Just making the argument more specific

  replaceWithStronger :: [Maybe [Char]] -> Char
  replaceWithStronger = replaceWithP

  -- Lifting Once

  liftedReplace :: Functor f => f a -> f Char
  liftedReplace = fmap replaceWithP

  liftedReplace' :: [Maybe [Char]] -> [Char]
  liftedReplace' = liftedReplace

  -- Lifting twice

  twiceLifted :: (Functor f1, Functor f2) => f2 (f1 a) -> f2 (f1 Char)
  twiceLifted = (fmap . fmap) replaceWithP

  -- making it more specific
  twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
  twiceLifted' = twiceLifted


  -- Lifting Thrice

  thriceLifted :: (Functor f1, Functor f2, Functor f3) => 
    f3 (f2 (f1 a)) -> f3 (f2 (f1 Char))
  thriceLifted = (fmap . fmap . fmap) replaceWithP

  -- explicit

  thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
  thriceLifted' = thriceLifted