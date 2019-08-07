module RPN where

import Data.List

solveRPN :: (Num a, Read a, Floating a) => String -> a
solveRPN expr =
  head $ foldl foldingFunc [] (words expr)
  where
    foldingFunc (x:y:xs) "*" = (x * y):xs
    foldingFunc (x:y:xs) "+" = (x + y):xs
    foldingFunc (x:y:xs) "-" = (y - x):xs
    foldingFunc (x:y:xs) "/" = (y / x):xs
    foldingFunc (x:y:xs) "^" = (y ** x):xs
    foldingFunc (x:xs) "ln" = (log x):xs
    foldingFunc xs "sum" = [sum xs]
    foldingFunc xs numberStr = (read numberStr): xs