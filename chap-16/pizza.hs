module Pizza where

  area :: Double -> Double
  area dia = pi * (dia / 2) * 2

  type Area = Double
  type Cost = Double
  type Pizza = (Area, Cost)

  costPerInch :: Pizza -> Double
  costPerInch (dia, cost) = cost / (area dia)

  comparePizza :: Pizza -> Pizza -> Pizza
  comparePizza p1 p2 = 
    if costp1 < costp2
      then p1
      else p2
      where
        costp1 = costPerInch p1
        costp2 = costPerInch p2

  describePizza :: Pizza -> String
  describePizza pizza@(size, cost) = 
    "The " ++ show size ++ " pizza " ++
      "is cheaper at " ++
      show costSqInch ++ 
      " per square Inch"
    where
      costSqInch = costPerInch pizza

  main :: IO ()
  main = do
    putStrLn "What is the size of the pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizza pizza1 pizza2
    putStrLn (describePizza betterPizza)