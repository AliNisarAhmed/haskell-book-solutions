module PityTheBool where

  import Data.Int

  data BigSmall
    = Big Bool
    | Small Bool
    deriving (Eq, Show)

  -- cardinality = 4

  data NumberOrBool 
    = Numba Int8
    | BoolyBool Bool deriving (Eq, Show)

  -- cardinality of Int8 is 127 + 128 + 1 = 256
  -- cardinality, with Bool, will become x2 = 512
  -- WRONG!
  -- for sumtypes, cardinality is added
  -- so cardinality is 256 + 2 = 258

  data Person 
    = Person { name :: String
             , age :: Int} deriving (Eq, Show)