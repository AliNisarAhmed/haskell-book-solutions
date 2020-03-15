module WriterMonadExample where

import           Control.Monad.Writer

type MyWriter = Writer [Int] String

example :: Writer [Int] String
example = do
  tell [1..3]
  tell [3..5]
  return "foo"

example2 :: Writer [Int] String
example2 = do
  tell []
  tell [1]
  tell [2]
  return "bar"

output :: (String, [Int])
output = runWriter example
