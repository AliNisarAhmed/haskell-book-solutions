module ForLoop where 

import Data.Foldable (for_)
import Data.Traversable (for)

import Control.Monad (when)

-- main = do 
--   putStr "Numbers": 
--   for_ [1..5] $ \i -> 
--     do 
--       putStr " "
--       putStr (show i)
--   putStr "\n"