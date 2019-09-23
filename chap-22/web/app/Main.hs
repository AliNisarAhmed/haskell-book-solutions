{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)

-- scotty :: Port -> ScottyM () -> IO ()
-- get :: RoutePattern -> ActionM () -> ScottyM ()

data User
  = User {
    userId :: Int
  , userName :: String
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

ali :: User
ali = User { userId = 1, userName = "ali" }

sam :: User
sam = User { userId = 2, userName = "sam" }

allUsers :: [User]
allUsers =  [ali, sam]

main :: IO ()
main = do
  putStrLn "Starting Server"
  scotty 3000 $ do
    get "/users" $ do
      json allUsers
