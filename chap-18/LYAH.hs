module LYAH where

import Data.Char
import Control.Monad
import System.Random

-- main :: IO ()
-- main = do
--   putStrLn "Wat is your name"
--   name <- getLine
--   putStrLn $ "oh, so YOU are " ++ name
--   getLine
--   return ()

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem c vowels

capVowels :: String -> String
capVowels [] = []
capVowels (x:xs) =
  if isVowel x
  then (toUpper x): capVowels xs
  else x: capVowels xs

listMaybeSentences :: [Maybe String]
listMaybeSentences = [Just "ali", Just "ahmed", Just "sam", Nothing]

-- main :: IO ()
-- main = do
--   a <- listMaybeSentences
--   b <- a
--   let list = [b]
--   return ()

myMain = do
  a <- listMaybeSentences
  let (Just str) = fmap capVowels a
  let x = fmap putStrLn [str]
  x

-- main = sequence myMain

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   c <- getChar
--   if c == ' '
--     then return ()
--     else do
--       putChar c
--       putChar '\n'
--       main

-- main = do
--   colors <- forM [1, 2, 3, 4] (\a -> do
--     putStrLn $ "Which color do you associate with this number: " ++ show a ++ "?"
--     color <- getLine
--     return color
--     )
--   putStrLn "The colors you associated are: "
--   mapM putStrLn colors

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins g =
  let
    (firstCoin, newGen) = random g
    (secondCoin, secondGen) = random newGen
    (thirdCoin, thirdGen) = random secondGen
  in
    (firstCoin, secondCoin, thirdCoin)

finiteRandom :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandom 0 g = ([], g)
finiteRandom n gen =
  let
    (value, newGen) = random gen
    (restOfList, finalGen) = finiteRandom (n - 1) newGen
  in
    (value:restOfList, finalGen)


main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number from 1-10 are u thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen