{-# LANGUAGE OverloadedStrings #-}

-- From the article below
-- https://two-wrongs.com/a-gentle-introduction-to-monad-transformers.html

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError
  = InvalidEmail
  | NoSuchUser
  | WrongPassword
  deriving Show


getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
        [name, domain] ->
            Right domain
        _ ->
            Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult =
  T.putStrLn . either (const "Error: Invalid Domain") (append "Domain: ")

-----------------

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address: "
  email <- T.getLine
  return (getDomain email)

users :: Map Text Text
users = Map.fromList
  [ ("example.com", "abc123")
  , ("gmail.com", "google123")
  ]

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password: "
          password <- T.getLine
          if password == userpw
          then return token
          else
            return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left ->
      return left

-------------------------------------------------

data EitherIO e a
  = EitherIO {
    runEitherIO :: IO (Either e a)
  }

instance Functor (EitherIO e) where
  fmap f = EitherIO . (fmap . fmap) f . runEitherIO

-- instance Monoid e => Applicative (EitherIO e) where
--   pure x = EitherIO (pure (Right x))
--   (<*>) eitherIOF eitherIOX = do
--     eitherFunc <- eitherIOF
--     eitherX <- eitherIOX
--     EitherIO $ return (eitherFunc <*> eitherX)

----------------------------------------------------

data JustRight e a
  = JustRight {
    giveMe :: Maybe (Either e a)
  } deriving (Eq, Show)

instance Functor (JustRight e) where
  fmap f = JustRight . (fmap . fmap) f . giveMe

-- instance Monoid e => Applicative (JustRight e) where
--   pure x = JustRight (Just (Right x))
--   (<*>) jrFunc jrX =