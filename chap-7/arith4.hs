module Arith4 where

  -- roundTrip :: (Show a, Read a) => a -> a
  -- roundTrip = read . show

  -- main = 
  --   do 
  --     print (roundTrip 4)
  --     print (id 4)

  roundTrip :: (Show a, Read b) => a -> b
  roundTrip x = y
    where 
      string = show x
      y = read string 


  main = 
    do 
      print (roundTrip 4 :: Int)
      print (id 4)