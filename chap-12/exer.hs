module Exer where



  -- notThe "the" -> Nothing
  -- notThe "blahtheblah" -> Just "blahtheblah"
  notThe :: String -> Maybe String
  notThe "the" = Nothing
  notThe x = Just x

  replaceThe :: String -> String
  replaceThe str = unwords $ map replacer (words str)
    where 
      replacer :: String -> String
      replacer word = 
        case notThe word of 
          Nothing -> "a"
          Just x -> x

  startsWithVowel :: String -> Bool
  startsWithVowel (s:ss) = isVowel s


  countList :: [String] -> Integer
  countList [] = 0
  countList (s:ss) =
    if s == "the" && startsWithVowel (head ss) == True
    then 1 + countList ss
    else 0 + countList ss

  countTheBeforeVowel :: String -> Integer  
  countTheBeforeVowel = countList . words



  -- countVowels :: String -> Integer
  
  isVowel :: Char -> Bool
  isVowel = flip elem $ "aeiou"
      
  returnVowels :: String -> String
  returnVowels = filter isVowel

  countVowels :: String -> Integer
  countVowels str =  toInteger $ length $ returnVowels str


  -- validate the word

  newtype Word' = Word' String deriving (Eq, Show)
  vowels = "aeiou"
  mkWord :: String -> Maybe Word'
  mkWord str =
    case countV > countC of 
      True -> Nothing
      _ -> Just (Word' str)
    where
      countV = countVowels str
      countC = toInteger (length str) - countV
   