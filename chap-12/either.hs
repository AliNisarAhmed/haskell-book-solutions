module EitherType where

  type Name = String
  type Age = Integer

  data Person 
    = Person Name Age deriving (Eq, Show)
  
  data PersonInvalid 
    = NameEmpty
    | AgeTooLow deriving (Eq, Show)

  type ValidatePerson a =
    Either [PersonInvalid] a

  ageOkay :: Age -> Either [PersonInvalid] Age
  ageOkay age =
      case age >= 0 of 
        True -> Right age
        False -> Left [AgeTooLow]
  
  nameOkay :: Name -> Either [PersonInvalid] Name
  nameOkay name =
      case name == "" of 
        True -> Left [NameEmpty]
        False -> Right name

  mkPerson :: Name -> Age -> Either PersonInvalid Person
  mkPerson name age 
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Right $ Person name age

  mkPerson2 :: Name -> Age -> ValidatePerson Person  -- validatePerson is just a type alias for Either
  mkPerson' (nameOkay name) (ageOkay age)

  mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
  mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
  mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
  mkPerson' (Left badName) _             = Left badName
  mkPerson' _ (Left badAge)              = Left badAge