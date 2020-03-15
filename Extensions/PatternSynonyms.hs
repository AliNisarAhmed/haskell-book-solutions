{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- pattern synonyms are used when in pattern matching, a pattern arises again and again
-- we can then create a pattern synonym or pattern alias to re-use that logic

-- All code, comments and examples from https://jsdw.me/posts/haskell-language-extensions/

module PatternSynonyms where

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Show)

data Time
  = Time {
    hour   :: Int
  , minute :: Int
  } deriving (Eq, Show)

data DayTime
  = DayTime {
    day  :: Day
  , time :: Time
  } deriving (Eq, Show)

printTime :: Time -> String
printTime (Time h m) = pad (show h) ++ ":" ++ pad (show m)
  where
    pad k@(_:_:_) = k
    pad k@(_:_)   = "0" ++ k

printDayTime :: DayTime -> String
printDayTime (DayTime d t) = "It's " ++ show d ++ ", " ++ printTime t

-- basic pattern aliases for certain DayTimes
pattern SundayNoon = DayTime Sunday (Time 12 00)
pattern MidnightFriday = DayTime Friday (Time 00 00)

pattern Sun h m = DayTime Sunday (Time h m)
pattern Mon h m = DayTime Monday (Time h m)

-- So far, there has been a one to one mapping between pattern and data. Thus, we can treat patterns as if they were data. Sun 12 00 == SundayNoon == DayTime Sunday (Time 12 00). However, some patterns are one way; one pattern could match many values. One way patterns use <- instead of = to denote this.

-- match any DayTime that is sunday or monday using these
pattern AnyTimeSun <- DayTime Sunday _
pattern AnyTimeMon <- DayTime Monday _

whichDay :: DayTime -> String
whichDay AnyTimeSun = "Sunday! Yayyy!"
whichDay AnyTimeMon = ":("
whichDay _          = "meh"

whichDay' :: DayTime -> String
whichDay' d@AnyTimeSun = "Oh yes, Sunday, " ++ printTime (time d)
whichDay' d@AnyTimeMon = "Oh no, Monday, " ++ printTime (time d)
whichDay' _            = "Other."

pattern SunT t <- DayTime Sunday t
pattern MonT t <- DayTime Monday t

whichDay'' :: DayTime -> String
whichDay'' (SunT t)      = "Oh yes Sunday Again!!! " ++ printTime t
whichDay'' (MonT t)      = "Oh no Not monday again!!! " ++ printTime t
whichDay'' (DayTime _ t) = "Meh, just another day " ++ printTime t


-- One of the really cool things about PatternSynonyms is that they can be used in conjunction with ViewPatterns to run arbitrarily complex functions at match time. Here we run isMorningHour and see whether the answer matches True:

isMorningHour :: Int -> Bool
isMorningHour t = t < 12

pattern Morning <- DayTime _ (Time (isMorningHour -> True) _)  -- we dont care about Day, Time has hour such that isMorningHour == True, and we dont care abt minutes

timeOfDay :: DayTime -> String
timeOfDay Morning = "It is morning indeed"
timeOfDay _       = "Go back to sleep"


-- View patterns can also give back arbitrary variables. here we expect a tuple of (True,h) back, where we allow h to be made use of at match time.

isMorningHour' :: Int -> (Bool, Int)
isMorningHour' h = (h < 12, h)

pattern MorningT h m <- DayTime _ (Time (isMorningHour' -> (True, h)) m)

timeOfDay' :: DayTime -> String
timeOfDay' (MorningT h m) = "It's morning again " ++ show h ++ ": " ++ show m
timeOfDay' _              = "It is not morning, back to sleep"
