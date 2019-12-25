{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NewType where

  class TooMany a where
    tooMany :: a -> Bool

  instance TooMany Int where
    tooMany n = n > 42

  newtype Goats = Goats Int deriving (Eq, Show, TooMany)

  
  -- Due to the pragma above, we do not have to do this.

  -- instance TooMany Goats where
  --   tooMany (Goats n) = tooMany n 
