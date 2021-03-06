module SquareCube where

  mySqr = [x^2 | x <- [1..5]]
  myCube = [y^3 | y <- [1..5]]

  expr = [(x, y) | x <- mySqr, y <- myCube]

  expr2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]