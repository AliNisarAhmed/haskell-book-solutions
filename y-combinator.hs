module AlmostFactorial where

  almostFactorial f n 
    | n == 0 = 1
    | otherwise = n * f (n - 1)

  factorial0 =
    almostFactorial id

  factorial1 = 
    (almostFactorial factorial0)

  factorial2 =
    (almostFactorial factorial1)

  factorial3 =
    (almostFactorial factorial2)

  factorial4 =
    (almostFactorial factorial3)

  y f =
    f (y f)

  factorial = y almostFactorial