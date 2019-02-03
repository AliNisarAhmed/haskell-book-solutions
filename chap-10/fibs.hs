module NthFibonacci where

  fibs = 1 : scanl (+) 1 fibs

  fibsN n = fibs !! n

  fibs20 = take 20 fibs

  fibsLT100 = takeWhile (<100) fibs

  factorial = 1 : scanl (*) 1 [2..]