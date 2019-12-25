module Where where
  result = 
    x * 3 + y
    where
      x = 3
      y = 1000

  result2 =
    x * 5
    where 
      x = 10 * 5 + y
      y = 10

  result3 = 
    z / x + y
    where
      x = 7
      y = negate x
      z = y * 10