module LeanParsers where 

import Text.Trifecta

stop :: Parser a 
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwoThree = char '1' >> char '2' >> char '3'

oneTwoThreeEnd :: Parser ()
oneTwoThreeEnd = oneTwoThree >> eof

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = 
  print $ parseString p mempty "1234"

testParse2 :: Parser () -> IO ()
testParse2 p = 
  print $ parseString p mempty "123"

-- parser3 :: Parser String -> IO ()
-- parser3 p = 
--   parseString p mempty "1"

-- p123 :: Parser String 
-- p123 = string "1" <|> string "12" <|> string "123"

-- pnl s = putStrLn ('\n' : s)

-- main = do 
--   pnl "Parse full then EOF"
--   testParse2 oneTwoThreeEnd

-- main = do 
--   pnl "full:"
--   testParse oneTwoThree
--   pnl "stop:"
--   testParse stop

  -- pnl "one:"
  -- testParse one

  -- pnl "oneTwo:"
  -- testParse oneTwo

  -- pnl "oneTwo':"
  -- testParse oneTwo