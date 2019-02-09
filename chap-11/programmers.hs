module Programmers where

  import Data.List

  data OperatingSystem
    = Linux
    | BSD
    | Mac
    | Windows deriving (Eq, Show)

  data ProgLang
    = Haskell
    | Agda
    | Idris
    | Purescript deriving (Eq, Show)

  data Programmer
    = Programmer { os :: OperatingSystem
                 , lang :: ProgLang } deriving (Eq, Show)

  allOperatingSystems :: [OperatingSystem]
  allOperatingSystems = 
    [ Linux
    , BSD
    , Mac
    , Windows
    ]

  allLanguages :: [ProgLang]
  allLanguages = 
    [Haskell, Agda, Idris, Purescript]

  allProgrammers :: [OperatingSystem] -> [ProgLang] -> [Programmer]
  allProgrammers os pr = concat $ zipWith (\a bs -> map a bs) (map Programmer os) (permutations pr)