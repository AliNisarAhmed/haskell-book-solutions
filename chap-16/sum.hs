module SumHS where

  import System.Environment

  main :: IO ()
  main = do
    args <- getArgs
    mapM_ putStrLn args

  -- main :: IO ()
  -- main = do
  --   vals <- mapM (\_ -> getLine) [1..3]
  --   mapM_ putStrLn vals