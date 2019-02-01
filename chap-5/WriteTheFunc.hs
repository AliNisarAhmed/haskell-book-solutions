module WriteTheFunc where
  myFun :: (x -> y)
        -> (y -> z)
        -> c
        -> (a, x)
        -> (a, z)
  myFun xToY yToZ _ (a, x) = 
    (a, z)
      where
        y = xToY x
        z = yToZ y

  i :: a -> a
  i = (\x -> x) 

  c :: a -> b -> a
  c x y = x

  c'' :: b -> a -> b
  c'' x y = x

  c' :: a -> b -> b
  c' x y = y

  r :: [a] -> [a]
  -- r (x: xs) = xs
  r (x: _) = [x]

  co :: (b -> c) -> (a -> b) -> a -> c
  co bToC aToB a = 
    bToC (aToB a)

  a :: (a -> c) -> a -> a
  a _ x = x

  a' :: (a -> b) -> a -> b
  a' aToB a = aToB a