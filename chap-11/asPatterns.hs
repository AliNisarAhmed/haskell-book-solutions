module AsPatterns where

  import Data.Char

  isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
  isSubseqOf _ [] = False
  isSubseqOf [] _ = True
  isSubseqOf sub@(x:xs) seq@(y:ys)
    | elem x seq = isSubseqOf xs (dropWhile (/= x) seq)
    | otherwise = False
      

  capitalizeWords :: String -> [(String, String)]
  capitalizeWords str = 
    map (\word@(x:xs) -> (,) word ((toUpper x) : xs)) $ words str

  capitalizeWord :: String -> String
  capitalizeWord (x:xs) = (toUpper x) : xs

  capitalizeParagraph :: String -> String
  capitalizeParagraph str = unwords (map findAndCapitalize (words str))
    where
      findAndCapitalize word
        | (last word) == '.' = capitalizeWord word
        | otherwise = word
