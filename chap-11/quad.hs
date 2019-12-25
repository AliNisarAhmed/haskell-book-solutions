module Quad where

  data Quad 
    = One
    | Two
    | Three
    | Four 
    deriving (Eq, Show)

  -- 1. 
  eQuad :: Either Quad Quad
  -- forms = 4 + 4 = 8

  -- 2.
  prodQuad :: (Quad, Quad)
  -- forms = 4 * 4 = 16

  -- 3.
  funcQuad :: Quad -> Quad
  -- forms = 4 ^ 4 = 256


  -- 4.
  prodTBool :: (Boo, Bool, Bool)
  -- forms = 2 * 2 * 2 = 8

  -- 5.
  gTwo :: Bool -> Bool -> Bool
  -- forms = 2 ^ 2 ^ 2 = 16


  -- 6.
  fTwo :: Bool -> Quad -> Quad
  -- 2 -> 4 -> 4
  -- a -> b -> c = c ^ (a + b)
  -- => 4 ^ (2 * 4) = 65536