{-# LANGUAGE OverloadedStrings #-}

-- From the article below
-- https://two-wrongs.com/a-gentle-introduction-to-monad-transformers.html

import Data.Text
import Control.Monad

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError
  = InvalidEmail
  | NoSuchUser
  | WrongPassword
  deriving (Eq, Show, Ord)

instance Semigroup LoginError where 
  (<>) x y = x

instance Monoid LoginError where 
  mempty = InvalidEmail
  mappend = (<>)
  


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

-- getToken :: IO (Either LoginError Text)
-- getToken = do
--   T.putStrLn "Enter email address: "
--   email <- T.getLine
--   return (getDomain email)

-- users :: Map Text Text
-- users = Map.fromList
--   [ ("example.com", "abc123")
--   , ("gmail.com", "google123")
--   ]

-- userLogin :: IO (Either LoginError Text)
-- userLogin = do
--   token <- getToken
--   case token of
--     Right domain ->
--       case Map.lookup domain users of
--         Just userpw -> do
--           T.putStrLn "Enter password: "
--           password <- T.getLine
--           if password == userpw
--           then return token
--           else
--             return (Left WrongPassword)
--         Nothing -> return (Left NoSuchUser)
--     left ->
--       return left

-------------------------------------------------

data EitherIO e a
  = EitherIO {
    runEitherIO :: IO (Either e a)
  }

instance Functor (EitherIO e) where
  fmap f = EitherIO . (fmap . fmap) f . runEitherIO

instance Monoid e => Applicative (EitherIO e) where
  pure x = EitherIO (pure (Right x))
  (<*>) (EitherIO ioEf)  (EitherIO ioEx) = do
    EitherIO $ (liftA2 . liftA2) ($) ioEf ioEx

instance Monoid e => Monad (EitherIO e) where 
  return x = EitherIO (return $ Right x)
  eIO >>= f = do 
    e <- eIO
    f e


----------------------------------------------------

-- getToken using EitherIO

-- getToken :: EitherIO LoginError Text 
-- getToken = do 
--   EitherIO (fmap Right (T.putStrLn "Enter email address"))
--   input <- EitherIO (fmap Right (T.getLine))
--   EitherIO (return (getDomain input))

-- we need a way to lift IO and Either into EitherIO

liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . return 

liftIO :: IO a -> EitherIO e a 
liftIO = EitherIO . fmap Right

-- getToken re-written using Lifting funcitons

getToken :: Either LoginError Text 
getToken = do 
  liftIO $ T.putStrLn "Enter email address"
  input <- liftIO T.getLine
  liftEither (getDomain input)