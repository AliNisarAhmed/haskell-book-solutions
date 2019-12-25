module Exer where

  data Person 
    = Person Bool deriving (Show)
  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  data Mood 
    = Blah
    | Woot deriving (Show, Eq)
  settleDown x = 
    if x == Woot 
    then Blah
    else x
  
  -- In the above, Blah > Woot does not work
  -- because Ord has not been derived

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence = 
    Sentence Subject Verb Object
    deriving (Show, Eq)
  
  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  -- 

  data Rocks
    = Rocks String 
    deriving (Eq, Show)
  data Yeah
    = Yeah Bool 
    deriving (Eq, Show)
  data Papu 
    = Papu Rocks Yeah
    deriving (Eq, Show)

  -- phew = Papu "chases" True  -- does not comppile

  truth = Papu (Rocks "chomskydoz") (Yeah True)

  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

  -- comparePapus :: Papu -> Papu -> Bool
  -- comparePapus p p' = p > p'
  -- Above will not compile bcoz no instance of Ord