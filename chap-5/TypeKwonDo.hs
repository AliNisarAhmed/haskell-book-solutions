module TypeKwonDo where
  f :: Int -> String
  f = undefined

  g :: String -> Char
  g = undefined

  h :: Int -> Char
  h int = g (f int)

  --

  data A
  data B
  data C

  q :: A -> B
  q = undefined

  w :: B -> C
  w = undefined

  e :: A -> C
  e a = w (q a) 

  --

  data X
  data Y
  data Z

  xz :: X -> Z
  xz = undefined

  yz :: Y -> Z
  yz = undefined

  xform :: (X, Y) -> (Z, Z)
  xform (x, y) = (xz x, yz y)

  --

  munge :: (x -> y)
        -> (y -> (w, z))
        -> x
        -> w
  munge xToY yToWZ x = 
    w
      where
        (w, _) = yToWZ (xToY x)
    
    -- Multiple ways to write the above
    -- let
    --   y = xToY x
    --   (w, _) = yToWZ y
    -- in 
    --   w