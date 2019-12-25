module Reverse where
  rvrs :: String -> String
  rvrs str = 
    last ++ " " ++ mid ++ " " ++ first
    where
      first = take 5 str
      mid = take 2 (drop 6 str)
      last = drop 9 str
  
  main :: IO ()
  main = print $ rvrs "Curry is Awesome"
  -- or
  -- main = print ( rvrs "Curry is Awesome" )