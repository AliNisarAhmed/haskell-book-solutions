module SimpleJSON where

import           Data.List (intercalate)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

languages :: JValue
languages = JArray [JString "JS", JString "Haskell", JString "Elm"]

spouse :: JValue
spouse = JObject [("firstName", JString "Samrah"), ("lastName", JString "Akber"), ("languages", JArray [JString "Urdu", JString "English"])]

sampleJson =
  JObject [ ("firstName", JString "Ali"), ("lastName", JString "Ahmed"), ("languages", languages), ("spouse", spouse) ]

-- This JSON renderer will not pretty print the JSON

-- renderJValue :: JValue -> String
-- renderJValue (JString s) = s
-- renderJValue (JNumber x) = show x
-- renderJValue (JBool True) = "true"
-- renderJValue (JBool False) = "false"
-- renderJValue (JNull) = "null"
-- renderJValue (JObject list) =
--   "{ " ++ pairs list ++ " }"
--   where
--     pairs []  = ""
--     pairs kvp = intercalate ", " (map renderPair kvp)
--     renderPair (k, v) = k ++ ": " ++ renderJValue v
-- renderJValue (JArray list) =
--   "[ " ++ values list ++ " ]"
--   where
--     values [] = ""
--     values vs = intercalate ", " (map renderJValue vs)

-- putJValue :: JValue -> IO ()
-- putJValue v = putStrLn (renderJValue v)

