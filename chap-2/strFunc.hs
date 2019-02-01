module StrFunc where
  
  sample = "Curry is awesome!"

  appendExcl str = str ++ "!"

  only4th str = take 1 (drop 4 str)

  drop9 str = drop 9 str

  thirdLetter :: String -> Char
  thirdLetter str =
    head (drop 2 str)

  letterIndex :: Int -> Char
  letterIndex x = 
    head (drop (x - 1) "Curry is Awesome!")

  rvrsSpecific :: String -> String
  rvrsSpecific str =
    last ++ " " ++ mid ++ " " ++ first
    where
      first = take 5 str
      mid = take 2 (drop 6 str)
      last = drop 9 str