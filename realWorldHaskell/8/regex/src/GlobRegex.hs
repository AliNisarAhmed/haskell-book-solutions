module GlobRegex where

import           Data.Char        (toLower)
import           Text.Regex.Posix ((=~))

data Case
  = Sensitive
  | InSensitive deriving (Eq, Show, Ord)

globToRegex :: Case -> String -> String
globToRegex c cs
  | c == Sensitive = match
  | otherwise = map toLower match
  where
  match =  "^" ++ globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""              = ""
globToRegex' ('*': cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?': cs)       = '.' : globToRegex' cs
globToRegex' ('[': '!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)      = '[' : c : charClass cs
globToRegex' ('[':_)         = error "unterminated character class"
globToRegex' (c:cs)          = escape c ++ globToRegex' cs

escape :: Char -> String
escape c
  | c `elem` regexChars = '\\': [c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']': globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: Case -> FilePath -> String -> Bool
matchesGlob c name pat
  | c == InSensitive = (map toLower name ) =~ globToRegex c pat
  | otherwise = name =~ globToRegex c pat

