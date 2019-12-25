module PhoneExercise where

  import Data.List 
  import Data.Char

  data DaPhone 
    = DaPhone [(Char, String)] deriving Show

  phone =
    DaPhone [ ('*', "^")
            , ('#', ".,")
            , ('0', "+_")
            , ('1', "")
            , ('2', "abc")
            , ('3', "def")
            , ('4', "ghi")
            , ('5', "jkl")
            , ('6', "mno")
            , ('7', "pqrs")
            , ('8', "tuv")
            , ('9', "wxyz") ]

  convo :: [String]
  convo = 
    ["Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "lol ok, Have you ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. your turn"
    , "Ok, Do u think I am pretty lol"
    , "lol ya"
    , "Just making sure rofl ur turn"]

  type Digit = Char
  type Presses = Int

  reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
  reverseTaps (DaPhone phn@(x:xs)) char
    | isUpper char = [('*', 1)] ++ reverseTaps (DaPhone phn) (toLower char) 
    | elem char (snd x) = [ ( (fst x), (findIndexOf char (snd x)) + 1 ) ]
    | otherwise = reverseTaps (DaPhone xs) char
      where
        findIndexOf :: Char -> String -> Int
        findIndexOf chr list =
            case elemIndex chr list of
              Just x -> x
              Nothing -> 0

  cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
  cellPhonesDead phn = concatMap (reverseTaps phn) 

  fingerTaps :: [(Digit, Presses)] -> Presses
  fingerTaps = foldr (\(_ , p) b -> p + b) 0

  occurrences :: String -> Char -> Int
  occurrences str ch = foldr (\letter b -> if letter == ch then b + 1 else b) 0 str

  mostPopularLetter :: String -> Char
  mostPopularLetter str@(s:ss)= foldr (\a b -> if a /= ' ' && occurrences str a > occurrences str b then a else b ) s ss 

  costOfMostPopular :: String -> Int
  costOfMostPopular str@(s:ss) = (* occur) $ fingerTaps $ reverseTaps phone mostPop
              where
                mostPop = mostPopularLetter str
                occur = occurrences str mostPop

  coolestLetter :: [String] -> Char
  coolestLetter strings = mostPopularLetter (foldr (++) "" strings) -- Just join the strings and find the most popular of the combined

  coolestWord :: [String] -> String
  coolestWord strings@(s:ss) = 
    let 
      combined = words (unwords strings)
      occur str word = foldr (\a b -> if a == word then b + 1 else b) 0 str
      mostpop list@(x:xs) = foldr (\a b -> if occur list a > occur list b then a else b) x xs
    in 
      mostpop combined