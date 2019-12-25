module Fibonacci where

  -- nth fibonacci number
  -- nth fibanacci number is the sum of fibanocci n - 1 and fibonacci n - 2

  fibonacci :: Integral a => a -> a
  fibonacci 0 = 1
  fibonacci 1 = 1
  fibonacci n = 
    fibonacci (n - 1) + fibonacci (n - 2)
