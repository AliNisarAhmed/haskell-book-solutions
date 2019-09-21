module FmapFunc where

import Control.Applicative

multBy2 = (*2)
add10 = (+10)

comb :: Integer -> Integer
comb = multBy2 . add10

-- comb = \x -> multBy2 (add10 x)

comb2 :: Integer -> Integer
comb2 = fmap multBy2 add10

comb3 :: Integer -> Integer
comb3 = (+) <$> multBy2 <*> add10

comb4 :: Integer -> Integer
comb4 = do
  a <- multBy2
  b <- add10
  return (a + b)
  -- a and b are partially applied functions
  -- return (a + b) says:
  -- return, in a partially applied context, the partially applied functions a and b
  -- The argument (Integer) will be applied to a and b and their reuslt will be added together.
  -- return (a + b) is the partially applied function awaiting an argument.

  -- === OR ===

  -- We assign
-- the variable 𝑎 to the partially applied function multBy2, and 𝑏 to
-- add10. As soon as we receive an input, it will fill the empty slots
-- in boop and doop. The results will be bound to the variables 𝑎
-- and 𝑏 and passed into return.


comb5 :: Integer -> Integer
comb5 = multBy2 >>= (\a -> (add10 >>= (\b -> return (a + b))))