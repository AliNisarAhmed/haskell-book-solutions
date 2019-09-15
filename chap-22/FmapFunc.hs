module FmapFunc where

import Control.Applicative

multBy2 = (*2)
add10 = (+10)

comb :: Integer -> Integer
comb = multBy2 . add10

comb2 :: Integer -> Integer
comb2 = fmap multBy2 add10

comb3 = (+) <$> multBy2 <*> add10

comb4 = do
  a <- multBy2
  b <- add10
  return (a + b)