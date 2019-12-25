-- https://en.wikibooks.org/wiki/Haskell/Prologue:_IO,_an_applicative_functor

module MonadWikiBook where

import Data.Char
import Text.Read

calculateArea :: Float -> Float -> Float
calculateArea b h = 0.5 * b * h

askName :: IO ()
askName = do
  putStrLn "wHat iS yoUR NaMe? "
  name <- getLine
  let lowercase = map toLower name
  if elem lowercase ["simon", "john", "phil"]
    then putStrLn "Haskell is great"
    else
      if lowercase == "koen"
        then putStrLn "Debugging Haskell is good"
        else putStrLn "I dont know who you are"

askNumberAndDouble :: IO ()
askNumberAndDouble = do
  putStrLn "Give me a number to double"
  num <- getLine
  let converted = readMaybe num :: Maybe Double
  case fmap (*2) converted of
    Nothing -> do
      putStrLn "Give me a number, dumbo!"
      askNumberAndDouble
    Just x -> putStrLn $ show $ x


interactiveSumming :: IO ()
interactiveSumming = do
  putStrLn "Give me 2 numbers ok: "
  a <- getLine
  b <- getLine
  let numA = readMaybe a :: Maybe Double
      numB = readMaybe b :: Maybe Double
  case numA of
    Nothing -> retry
    Just x ->
      case numB of
        Nothing -> retry
        Just y ->
          putStrLn $ show (x + y)
    where
      retry = do
        putStrLn "Invalid number dumbo, retrying"
        interactiveSumming

interactiveSumming2 :: IO ()
interactiveSumming2 = do
  putStrLn "Give me 2 numbers, ok?: "
  x <- getLine
  y <- getLine
  let numA = readMaybe x :: Maybe Double
      numB = readMaybe y :: Maybe Double
  case (+) <$> numA <*> numB of
    Just sum -> putStrLn $ show sum
    Nothing -> do
      putStrLn "I suspect at least one of them is not a number, try again"
      interactiveSumming2

interactiveSumming3 :: IO ()
interactiveSumming3 = do
  putStrLn "Give me 2 numbers, sir: "
  numA <- readMaybe <$> getLine
  numB <- fmap readMaybe getLine
  case (+) <$> numA <*> numB :: Maybe Double of
    Just x -> putStrLn $ show x
    Nothing -> do
      putStrLn "Sorry but one of these is not a number, try again please."
      interactiveSumming3

interactiveConcat :: IO ()
interactiveConcat = do
  putStrLn "Give me two strings, any strings: "
  result <- (++) <$> getLine <*> (take 3 <$> getLine)  -- order is respected by <*>, only the second input will be truncated
  putStrLn "Joining them: "
  putStrLn result

interactiveConcat2 :: IO ()
interactiveConcat2 = do
  str <- putStrLn "Choose two strings: " *> ( (++) <$> getLine <*> getLine)  -- effectively, *> in do blocks is just line breaks, performing effects one after the other
  putStrLn "Lets join them: " *> putStrLn str

themselvesTimes :: [Int] -> [Int]
-- themselvesTimes m = do
--   x <- m
--   replicate x x
themselvesTimes m = m >>= (\x -> replicate x x)

alternative fs xs = [f x | f <- fs, x <- xs]

fs = [(+1), (+2), (+3)]
xs = [4, 5, 6]

main :: IO ()
main = do
  interactiveConcat2
  -- interactiveSumming3
  -- askNumberAndDouble
  -- askName

-- main :: IO ()
-- main = do
--   putStrLn "The base?"
--   baseString <- getLine
--   putStrLn "The height?"
--   heightString <- getLine
--   let b = read baseString :: Float
--       h = read heightString :: Float
--       area = calculateArea b h
--   putStrLn $ "The area of the trianlge is " ++ show area