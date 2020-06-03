module IntroToParsers where 

import Text.ParserCombinators.ReadP
import Control.Applicative

isVowel :: Char -> Bool 
isVowel char = 
    any (char == ) "aeiou"

vowel :: ReadP Char 
vowel = satisfy isVowel

atleastOneVowel :: ReadP [Char]
atleastOneVowel = many1 vowel

airport :: ReadP String 
airport = 
    many1 (satisfy (\char -> char >= 'A' && char <= 'Z'))

xcode = "BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195" 

airport2 :: ReadP String 
airport2 = do 
  code <- many1 (satisfy $ \char -> char >= 'A' && char <= 'Z')
  satisfy (== ' ')
  return code 

digit :: ReadP Char 
digit = 
  satisfy $ \c -> c >= '0' && c <= '9'

timestamp :: ReadP (Int, Int, Int)
timestamp = do 
  day <- numbers 2
  hour <- numbers 2 
  minute <- numbers 2  
  string "Z "
  if day < 1 || day > 31 || hour > 23 || minute > 59 
    then pfail
    else return (day, hour, minute)

numbers :: Int -> ReadP Int
numbers n = fmap read $ count n digit

----------------------------------------------------

windInfo :: ReadP WindInfo
windInfo = do 
  direction <- numbers 3 
  speed <- numbers 2 <|> numbers 3
  gust <- option Nothing (fmap Just gustParser)
  unit <- string "KT" <|> string "MPS"
  string " "
  return (WindInfo direction (toMPS unit speed) $ fmap (toMPS unit) gust)

toMPS :: String -> Int -> Int 
toMPS unit speed = 
  case unit of 
    "KT" -> div speed 2 
    "MPS" -> speed

gustParser :: ReadP Int 
gustParser = do 
  satisfy (== 'G')
  numbers 2 <|> numbers 3

data WindInfo
  = WindInfo 
    { dir :: Int 
    , speed :: Int 
    , gusts :: Maybe Int
    } deriving (Show)

--- implement the library's string function
-- string :: String -> ReadP String

myString :: String -> ReadP String 
myString (x:[]) = many1 $ satisfy (== x)
myString (x:xs) = do 
  p1 <- satisfy (== x)
  p2 <- myString xs
  return (p1: p2)

myString2 :: String -> ReadP String 
myString2 str = traverse (satisfy . (==)) str