{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LogicGoats where

  class TooMany a where
    tooMany :: a -> Bool

  instance TooMany Int where
    tooMany n = n > 42

  instance TooMany (Int, String) where
    tooMany (int, str) = int == 42 && str == "ali" 


  -- newtype Gloats = Gloats (Int, String) deriving (Eq, Show)

  -- instance TooMany Gloats where
  --   tooMany Gloats (int, str) = int == 42 && str == "ali"

  instance TooMany (Int, Int) where
    tooMany (int1, int2) = (int1 + int2) > 42

  instance TooMany (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)