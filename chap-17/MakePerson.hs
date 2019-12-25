module MakePerson where

validateLength :: Int -> String -> Maybe String
validateLength maxLength s =
  if (length s) > maxLength
  then Nothing
  else Just s

newtype Name
  = Name String deriving (Eq, Show)

newtype Address
  = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  fmap Name $ validateLength 5 s

mkAddress :: String -> Maybe Address
mkAddress a =
  fmap Address $ validateLength 15 a

data Person
  = Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a
  -- case mkName n of
  --   Nothing -> Nothing
  --   Just n' ->
  --     case mkAddress a of
  --       Nothing -> Nothing
  --       Just a' ->
  --         Just $ Person n' a'

---------------

data Cow
  = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'
  -- all the code below replaced by above!!!
  -- case noEmpty name' of
  --   Nothing -> Nothing
  --   Just name ->
  --     case noNegative age' of
  --       Nothing -> Nothing
  --       Just age ->
  --         case noNegative weight' of
  --           Nothing -> Nothing
  --           Just weight ->
  --             Just (Cow name age weight)
