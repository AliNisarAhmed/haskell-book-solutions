module DividedBy where

  dividedBy :: Integral a => a -> a -> (a, a)
  dividedBy numer denom = go numer denom 0
    where 
      go n d count
        | n < d = (n, count)
        | otherwise =
          go (n - d) d (count + 1)


  --dividedBy 15 2
  -- go 15 2 0
  -- go 13 2 1
  -- go 11 2 2
  -- go 9 2 3
  -- go 7 2 4
  -- go 5 2 5
  -- go 3 2 6
  -- go 1 2 7 (1, 7)