module Filter where

import           Data.Char (toUpper)
-- main = interact (unlines . filter (elem 'a') . lines)

-- run this file like this
-- stack runghc filter.hs                           -> Interactive
-- stack runghc filter.hs < input.txt               -> Write to screen
-- stack runghc filter.hs < input.txt > output.txt

main = interact $ map toUpper
