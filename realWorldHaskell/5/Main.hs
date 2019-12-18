module Main where

import JsonHaskell

main :: IO ()
main = print $ JObject [("foo", JNumber 1), ("bar", JBool False)]