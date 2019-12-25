module Applic where

import           Control.Applicative

a1 x =
  lookup x [ (3, "hello")
            , (4, "julie")
            , (5, "kabai")]

a2 y =
  lookup y [ (6, "sup?")
          ,(7, "chris")
          ,(8, "aloha")]

n1 z =
  lookup z [(3, 3), (4, 6), (5, 8)]

n2 x =
  lookup x [(6, 10), (7, 13),(8, 9001)]

------

data MT a = MT { getT :: a} deriving (Eq, Show)
