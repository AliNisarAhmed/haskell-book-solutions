module Actions where

str2Action :: String -> IO ()
str2Action str = putStrLn $ "Data: " ++ str

list2Actions :: [String] -> [IO ()]
list2Actions list = map str2Action list

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2Actions strings

printitall :: IO ()
printitall = runall actions

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (x:xs) = do
  x
  runall xs

main :: IO ()
main = do
  str2Action "Start of the program"
  printitall
  str2Action "Done!"
