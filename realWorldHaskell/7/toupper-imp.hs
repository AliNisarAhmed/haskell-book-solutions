module ToUpperImp where

import           Data.Char (toUpper)
import           System.IO

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inputStr <- hGetLine inh
      putStrLn (map toUpper inputStr)
      hPutStrLn outh (map toUpper inputStr)
      mainloop inh outh
