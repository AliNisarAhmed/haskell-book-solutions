module ThyFearfulSymmetry where

  myWords :: Char -> [Char] -> [[Char]]
  myWords sep str
    | str == "" = []
    | otherwise = [takeWhile (/= sep) str] ++ (myWords sep . drop 1 . dropWhile (/= sep)) str



    -- myWords (drop 1 (dropWhile (/= ' ') str))

  firstSen = "Tyger Tyger, burning bright\n"
  secondSen = "In the forests of the night\n"
  thirdSen = "What immortal hand or eye\n"
  fourthSen = "Could frame thy fearful\
    \ symmetry?"

  sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

  myLines :: String -> [String]
  myLines = myWords '\n'