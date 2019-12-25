module List where

data List a
  = Cons a (List a)
  | Nil deriving (Eq, Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: List a -> [a]
toList Nil = []
toList (Cons x list) = x : toList list

data Tree a = Just (Tree a) a (Tree a) | Nothing deriving (Eq, Show)