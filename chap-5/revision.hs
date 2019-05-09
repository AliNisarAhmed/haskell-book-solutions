{-# LANGUAGE NoMonomorphismRestriction #-}

module Revision where

  -- example = 1

  -- x = 5
  -- y = x + 5
  -- f = 4 / y

  -- bigNum = (^ 5)
  -- wahoo = bigNum (10 :: Int)

  -- y = print "wohoo"
  -- z = x "hello"

  -- a = (+)
  -- b = 5


  i :: a -> a
  i x = x

  c :: a -> b -> a
  c x y = x

  c' :: a -> b -> b
  c' _ y = y

  r :: [a] -> [a]
  r [] = []
  r (x:xs) =  (r xs) ++ [x]
  
  r2 :: [a] -> [a]
  r2 (_:xs) = xs

  co :: (b -> c) -> (a -> b) -> a -> c
  co bToC aToB a = bToC $ aToB $ a

  a :: (a -> c) -> a -> a
  a _ x = x

  a' :: (a -> b) -> a -> b
  a' aToB a = aToB $ a

  fstString :: [Char] -> [Char]
  fstString x = x ++ " in the rain"

  sndString :: [Char] -> [Char]
  sndString x = x ++ " over the rainbow"

  sing = 
    if (x < y) then fstString x else sndString y
      where
        x = "Singin"
        y = "Somewhere"

  main :: IO ()
  main = do
    print $ 1 + 2
    putStrLn "10"
    print $ show (negate $ -1)
    print ((+) 0 blah)
    where
      blah = negate 1

  data Woot

  f :: Int -> String
  f = undefined

  g :: String -> Char
  g = undefined

  h :: Int -> Char
  h int = g . f $ int

  -------------------------------

  data A
  data B
  data C

  q :: A -> B
  q = undefined

  w :: B -> C
  w = undefined

  e :: A -> C
  e a = w . q $ a
  
  ------------------------------------

  data X
  data Y
  data Z

  xz :: X -> Z
  xz = undefined

  yz :: Y -> Z
  yz = undefined

  xform :: (X, Y) -> (Z, Z)
  xform (x, y) = (xz x, yz y)

  munge :: (x -> y)
        -> (y -> (w, z))
        -> x
        -> w
  munge xToY yWZ x = fst tuple
    where
      y = xToY x
      tuple = yWZ y

