module ProductTypes where

  -- data Fiction = F deriving Show
  -- data NonFiction = NF deriving Show

  -- data BookType
    -- = FictionBook Fiction
    -- | NonfictionBook NonFiction

  type AuthorName = String

  -- data Author = Author (AuthorName, BookType)

  data Author 
    = Fiction AuthorName
    | NonFiction AuthorName deriving (Eq, Show)