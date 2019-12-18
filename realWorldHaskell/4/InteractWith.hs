module InteractWith where

import System.Environment (getArgs)
import Data.Char
import Data.List

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  putStrLn $ input
  putStrLn $ "------"
  putStrLn $ function input
  writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "Error: exactly two arguments needed"
    myFunction = transposeWords


transposeWords :: String -> String
transposeWords = unlines . transpose . lines

-- transposeWords2 :: [String] -> String
-- transposeWords2 [] = []
-- transposeWords2 list@(x:xs) = map head list : (transposeWords2 xs)

firstWord :: String -> [Char]
firstWord str = map head $ splitLines str


splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in
    pre : case suf of
      ('\r':'\n':rest) -> splitLines rest
      ('\r':rest) -> splitLines rest
      ('\n':rest) -> splitLines rest
      _ -> []

isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines = unlines . splitLines