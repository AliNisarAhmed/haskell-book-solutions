module FiveSidedDieM where

  data FiveSidedDie
    = S1 
    | S2
    | S3
    | S4
    | S5 deriving (Eq, Show, Enum)

  class (Eq a, Enum a, Show a) => Die a where
    roll :: Int -> a
  
  instance Die FiveSidedDie where
    roll int = toEnum (int `mod` 3)
  