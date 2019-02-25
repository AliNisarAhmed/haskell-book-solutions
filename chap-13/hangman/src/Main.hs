module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, maybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

type WordList = [String]

allWords :: IO WordList 
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w = 
      let
        l = length (w :: String)
      in 
        l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle 
  = Puzzle String [Maybe Char] [Char]
    -- Puzzle (the word) (correct guesses) (wrong guesses)

instance Show Puzzle where
  show (Puzzle _ discovered wrongs) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Wrong Guessed so far: " ++ wrongs

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map f str) []
    where 
      f :: Char -> Maybe Char
      f _ = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _ ) = flip elem $ str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ corrects wrongs) c = (flip elem $ wrongs) c || (flip elem $ converted) c
  where
    converted = map f corrects
    f = maybe ' ' id

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '*'
renderPuzzleChar (Just x) = x

countJusts :: [Maybe Char] -> Int
countJusts [] = 0
countJusts (x:xs) = 
  case x of 
    Just _ -> 1 + countJusts xs
    _ -> 0 + countJusts xs

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = 
  Puzzle word newFilledInSoFar newS
    where
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar
      zipper guessed wordChar guessChar = 
        if wordChar == guessed
        then Just wordChar
        else guessChar
      newS = 
        if countJusts newFilledInSoFar > countJusts filledInSoFar
        then s
        else c : s

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You have already guessed that letter, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "Correct! filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Wrong! Try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
  if (length guessed) > 7 
  then
    do 
      putStrLn "You Lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else return ()
    
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
  if all isJust filledInSoFar then
    do
      putStrLn "You win!"
      exitSuccess
  else return () 

runGame :: Puzzle -> IO ()
runGame puzzle = 
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a Letter: "
    guess <- getLine
    case guess of 
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "You guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle