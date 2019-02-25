module MakePerson where

  type Name = String
  type Age = Integer

  data Person
    = Person Name Age deriving (Show)

  data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String deriving (Eq, Show)
  
  mkPerson :: Name -> Age -> Either PersonInvalid Person
  mkPerson name age 
   | name /= "" && age > 0 = Right $ Person name age
   | name == "" = Left NameEmpty
   | not (age > 0) = Left AgeTooLow
   | otherwise = Left $ PersonInvalidUnknown $ 
    "Name was: " ++ show name ++ " Age was: " ++ show age

  gimmePerson :: IO ()
  gimmePerson = do
    putStrLn "Enter name: "
    name <- getLine
    putStrLn "Enter Age: "
    ageChar <- getLine
    case mkPerson name (read ageChar :: Integer) of 
      Left x -> 
        case x of 
          NameEmpty -> do putStrLn "Name cannot be empty"
          AgeTooLow -> do putStrLn "Age cannot be zero or less"
          PersonInvalidUnknown x -> do putStrLn x
      Right (Person name age) -> 
        do putStrLn $ "Yay! Successfully got a person: " ++ "Person " ++ name ++ " " ++ ageChar 