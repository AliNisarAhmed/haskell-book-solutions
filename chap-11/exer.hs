module Exer where

  import Data.Char

  -- vigenere :: String -> String -> String
  -- vigenere code input =
  --   let
  --     codeRepeated = take (length input) (cycle code)
  --     subOrd = subtract 97 . ord
  --     mappedCode = map subOrd codeRepeated
  --     mappedInput = map subOrd input
  --     zipped = zipWith (+) mappedCode mappedInput
  --     rounder x 
  --       | x >= 26 = (mod x 26) + 97
  --       | otherwise = x + 97
  --     answer = map (chr . rounder) zipped
  --   in 
  --     answer

  vigenere code input =
      map (chr . rounder) $ zipWith (+) (map subOrd codeRepeated) (map subOrd input)
      where
        codeRepeated = take (length input) (cycle code)
        subOrd = subtract 97 . ord
        rounder x
          | x >= 26 = (mod x 26) + 97
          | otherwise = x + 97