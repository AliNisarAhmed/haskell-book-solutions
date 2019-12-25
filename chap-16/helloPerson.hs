module HelloPerson where

  import qualified Data.Map.Strict as M

  helloPerson :: String -> String
  helloPerson name = 
    "Hello " ++ name ++ "!"

  nameData :: M.Map Int String
  nameData = M.fromList [(1, "ali")]

  main :: Maybe String
  main = do
    name <- M.lookup 1 nameData
    let statement = helloPerson name
    return statement