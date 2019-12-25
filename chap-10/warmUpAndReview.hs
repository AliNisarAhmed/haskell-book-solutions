module WarmUp where

  stops = "pbtdkg"
  vowels = "aeiou"

  makeTuples :: String -> String -> [(Char, Char, Char)]
  makeTuples xs ys = [(s1, v1, s2) | s1 <- xs, s2 <- xs, v1 <- ys]

  nouns = ["ali", "sam", "icecream"]
  verbs = ["walk", "talk", "sleep"]

  makeTuples2 :: [a] -> [b] -> [(a, b, a)]
  makeTuples2 xs ys = [(x1, y1, x2) | x1 <- xs, x2 <- xs, y1 <- ys] 


  seekritFunc :: Fractional a => String -> a
  seekritFunc x = (fromIntegral (sum (map length (words x)))) / ( fromIntegral (length (words x)))