module MulWithSum where

  mulWithSum :: Integral a => a -> a -> a
  mulWithSum x y = go x y 0
    where 
      go num1 num2 result
      -- we keep decreasing num2, when it is zero we simply return the result
        | num2 == 0 = result   
      -- otherwise, we decrease num2 by 1, and add num1 to the result so far
        | otherwise = go num1 (num2 - 1) (result + num1)
      
  -- mulWithSum x 1 = x
  -- mulWithSum x y = x + mulWithSum x (y - 1)

