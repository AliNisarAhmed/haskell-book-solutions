module Lib where

import Data.List (sort)
import Data.Char (isAlpha)

areAnagrams :: String -> String -> String
areAnagrams word1 word2 =
    case isWord word1 of
        Nothing -> "The first word is invalid"
        Just word1 ->
            case isWord word2 of
                Nothing -> "The second word is invalid"
                Just word2 ->
                    case (isAnagram word1 word2) of
                        False -> "Not Anagrams"
                        True -> "These words are Anagrams"

isAnagram :: String -> String -> Bool
isAnagram word1 word2 =
    (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word =
    case null word of
        True -> Nothing
        False ->
            case (all isAlpha word) of
                False -> Nothing
                True -> Just word

-- main :: IO ()
-- main = do
--     putStrLn "Enter first word"
--     word1 <- getLine
--     putStrLn "Enter second word"
--     word2 <- getLine
--     print $ areAnagrams word1 word2