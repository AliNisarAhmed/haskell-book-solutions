module GrabBag where
  
  -- mth1 x y z = x * y * z
  -- mth2 x y = \z -> x * y * z
  -- mth3 x = \y -> \z -> x * y * z
  -- mth4 = \x -> \y -> \z -> x * y * z

  -- addOne = \x -> x + 1

  -- addOneIfOdd n =
  --   case odd n of
  --     True -> f n
  --     False -> n
  --     where
  --       f = addOne

  addFive x y = 
    (if x > y then y else x ) + 5
  -- addFive = 
  --   \x -> 
  --     \y ->
  --       (if x > y 
  --       then y
  --       else x) + 5 

  myFlip f x y = f y x