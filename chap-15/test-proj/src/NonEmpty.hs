module NonEmptyM where

  data NonEmpty a
    = a :| [a] deriving (Eq, Ord, Show)