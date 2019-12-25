module Revision where

  type Name = String
  type Age = Integer

  data Person 
    = Person Name Age deriving Show

  data PersonInvalid 
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

  mkPerson :: Name -> Age -> Either PersonInvalid Person
  mkPerson name age
    | name /= "" && age > 0 = 
      Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = 
      Left $ PersonInvalidUnknown $ 
        "Name was: " ++ show name ++ " Age was: " ++ show age

  gimmePerson :: IO ()
  gimmePerson = do
    putStrLn "Give me a name: " 
    name <- getLine
    putStrLn "Give me an age: "
    age <- getLine
    case mkPerson name (read age :: Integer) of
      Right x -> putStrLn $ "Success: " ++ show x
      Left y -> putStrLn $ "Failure: " ++ show y
        