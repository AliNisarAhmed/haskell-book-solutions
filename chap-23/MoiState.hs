{-# LANGUAGE InstanceSigs #-}

module MoiState where

newtype Moi s a
  = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\s -> (f $ fst (g s), s))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi (\s -> ((fst $ f s) (fst $ g s), s))

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= aToMoiB = Moi $ \s ->
    let
      (a, _) = f s
    in
      (runMoi $ aToMoiB a) s

  -- My Solution
  -- (Moi f) >>= g = Moi $ \s -> (fst $ (runMoi $ g (fst $ f s)) s, s)

  -- fst $ f s -> a
  -- fst $ (runMoi $ g a) s => (\s -> (b, s))