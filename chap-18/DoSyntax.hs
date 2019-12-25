module DoSyntax where

import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  (putStrLn "Hello" >> putStrLn "World") >> putStrLn "!"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "Yo" *> putStrLn "mama"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("Hello there: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls: " >>
  getLine >>=
    \name -> putStrLn ("Hello there: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn "age pls: "
  age <- getLine
  putStrLn ("hello there: " ++ name ++ "I know you age: " ++ age)

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls: " >>
  getLine >>=
    \name ->
      putStrLn "age pls: " >>
      getLine >>=
        \age ->
          putStrLn ("hello there " ++ name ++ "I know you age: " ++ age)


-----------------------------

-- twiceWhenEven :: [Integer] -> [Integer]
-- twiceWhenEven xs = do
--   x <- xs
--   if even x
--     then [x*x, x*x]
--     else [x*x]

-- vs

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []