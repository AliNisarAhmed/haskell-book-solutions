{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isAlphaNum, isSpace)

maxLength :: String -> Either String String
maxLength "" = Left "Password cannot be empty"
maxLength pwd =
  case (length pwd > 20) of
    True -> Left "Password too long, must be < 20"
    False -> Right pwd

allAlpha :: String -> Either String String
allAlpha "" = Left "Password cannot be empty"
allAlpha pwd =
  case (all isAlphaNum pwd) of
    False -> Left "Only Alphabets & Numbers"
    True -> Right pwd

stripSpace :: String -> Either String String
stripSpace "" = Left "Password cannot be empty"
stripSpace (x:xs) =
  case (isSpace x) of
    True -> stripSpace xs
    False -> Right (x:xs)

-- checkPassword :: String -> Maybe String
-- checkPassword "" = Nothing
-- checkPassword pwd =
--   case (stripSpace pwd) of
--     Nothing -> Nothing
--     Just p1 ->
--       case (allAlpha p1) of
--         Nothing -> Nothing
--         Just p2 ->
--           case (maxLength p2) of
--             Nothing -> Nothing
--             Just p3 -> Just p3

checkPassword :: String -> Either String String
checkPassword pwd =
  stripSpace pwd >>= allAlpha >>= maxLength

newtype Password =
  Password String deriving (Eq, Show)

newtype Error =
  Error String deriving (Eq, Show)

newtype Username =
  Username String deriving (Eq, Show)

passwordLength :: String -> Either Error Password
passwordLength "" = Left (Error "Password can't be empty")
passwordLength str =
  case (length str > 20) of
    True -> Left $ Error "Password too long, must be < 20"
    False -> Right $ Password str

usernameLength :: String -> Either Error Username
usernameLength "" = Left (Error "Password can't be empty")
usernameLength str =
  case length str > 15 of
    True -> Left $ Error "Username too long, must be < 15"
    False -> Right $ Username str

allAlpha2 :: String -> Either Error Password
allAlpha2 "" = Left $ Error "Password can't be empty"
allAlpha2 str =
  case all isAlphaNum str of
    False -> Left $ Error "Alphabest & Numbers only"
    True -> Right $ Password str

stripSpace2 :: String -> Either Error Password
stripSpace2 "" = Left $ Error "Password can't be empty"
stripSpace2 (x:xs) =
  case isSpace x of
    True -> stripSpace2 xs
    False -> Right $ Password (x:xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password pwd) =
  stripSpace2 pwd >>= allAlpha2 >>= passwordLength

main :: IO ()
main = do
  putStrLn "Give me Password"
  pwd <- Password <$> getLine
  print $ validatePassword pwd
