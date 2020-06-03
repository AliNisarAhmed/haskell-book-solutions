{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Char (isAlphaNum, isSpace)
import Data.Validation

newtype Password =
  Password String deriving (Eq, Show)

newtype Error =
  Error [String] deriving (Eq, Show)

-- This instance is needed by the Validation type of the left side
-- type parameter
instance Semigroup Error where 
  Error x <> Error y = Error (x <> y)

newtype Username =
  Username String deriving (Eq, Show)

data User 
  = User Username Password deriving (Eq, Show)

passwordLength :: String -> Validation Error Password
passwordLength ""  = Failure (Error ["Password can't be empty"])
passwordLength str = case (length str > 20) of
  True  -> Failure $ Error ["Password too long, must be < 20"]
  False -> Success $ Password str

usernameLength :: String -> Validation Error Username
usernameLength ""  = Failure (Error ["Password can't be empty"])
usernameLength str = case length str > 15 of
  True  -> Failure $ Error ["Username too long, must be < 15"]
  False -> Success $ Username str

allAlpha :: String -> Validation Error String
allAlpha "" = Failure $ Error ["Password can't be empty"]
allAlpha str = case all isAlphaNum str of
  False -> Failure $ Error ["Alphabest & Numbers only"]
  True  -> Success str

stripSpace :: String -> Validation Error String
stripSpace  ""       = Failure $ Error ["Password can't be empty"]
stripSpace (x:xs) = case isSpace x of
  True  -> stripSpace xs
  False -> Success (x : xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password pwd) =
  case (stripSpace pwd) of 
    Failure err -> Failure err 
    Success pwd' -> 
      allAlpha pwd' *> passwordLength pwd'

-- *> :: f a -> f b -> f b (discards the results, but keeps the effects)
-- Thus in Validation (*>) concats or mappends the error values if an erro
  -- is present

validateUsername :: Username -> Validation Error Username
validateUsername (Username name) =
  case (stripSpace name) of 
    Failure err -> Failure err
    Success name' -> 
      allAlpha name' *> usernameLength name'

passwordErrors :: Password -> Validation Error Password 
passwordErrors pwd = 
  case validatePassword pwd of 
    Failure err -> Failure (Error ["Password Error: "] <> err)
    Success p -> Success p

usernameErrors :: Username -> Validation Error Username 
usernameErrors name =
  case validateUsername name of 
    Failure err -> Failure (Error ["Username Error: "] <> err)
    Success n -> Success n

makeUser :: Username -> Password -> Validation Error User 
makeUser name pwd = 
  User <$> usernameErrors name <*> passwordErrors pwd

errorCoerce :: Error -> [String]
errorCoerce (Error s) = s

display :: Username -> Password -> IO ()
display name pwd = 
  case makeUser name pwd of 
    Failure err -> putStrLn $ unlines $ errorCoerce err
    Success (User (Username n) p) -> 
      putStrLn $ "Welcome, " ++ n

main :: IO ()
main = do
  putStrLn "Give me username"
  username <- Username <$> getLine
  putStrLn "Give me Password"
  pwd <- Password <$> getLine
  display username pwd
