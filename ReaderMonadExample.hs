module ReaderMonadExample where

import           Control.Monad.Reader

data MyContext =
  MyContext
    { foo :: String
    , bar :: Int
    } deriving (Eq, Show)

computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "Jello" (-1)

------

newtype MyReader r a = MyReader { run :: r -> a }

instance Monad (MyReader a) where
  return a = MyReader $ \_ -> a
  m >>= f = MyReader $ \r -> run (f (run m r)) r

ask :: MyReader a a
ask = MyReader id

asks :: (r -> a) -> MyReader r a
asks f = MyReader f

local :: (r -> r) -> MyReader r a -> MyReader r a
local f m = MyReader $ run m . f
