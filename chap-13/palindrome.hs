module Palindrome where

  import Data.Char (toLower)
  import Control.Monad
  import System.Exit (exitSuccess)

  palindrome :: IO ()
  palindrome = forever $ do
  line1 <- getLine
  case ( (combined line1) == reverse (combined line1)) of
    True -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope!" 
  where
    isAlpha = flip elem $ "abcdefghijklmnopqrstuvwxyz"
    filterAlphabet = filter isAlpha
    mapToLower = map toLower
    combined = filterAlphabet . mapToLower
