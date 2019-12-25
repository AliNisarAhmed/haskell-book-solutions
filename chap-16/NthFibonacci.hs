module NthFibonacci where

  nthfibonacci :: Int -> Int
  nthfibonacci 0 = 0
  nthFibonacci 1 = 1
  nthFibonacci 2 = 1
  nthFibonacci n = nthFibonacci (n - 1) + nthFibonacci (n - 2)

  main :: IO ()
  main = do
    putStrLn "Enter a number (n), I will show you the nth fibonacci"
    num <- getLine
    let result = nthFibonacci (read num :: Int)
    putStrLn $ show result