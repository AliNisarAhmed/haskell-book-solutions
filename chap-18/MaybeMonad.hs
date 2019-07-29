module MaybeMonad where

data Cow
  = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n < 0 = Nothing
  | otherwise = Just n

weightCheck :: Cow -> Maybe Cow
weightCheck c@(Cow name _ w) =
  if name == "Bess" && w > 499
    then Nothing
    else Just c

-- mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow n a w =
--   case noEmpty n of
--     Nothing -> Nothing
--     Just name ->
--       case noNegative a of
--         Nothing -> Nothing
--         Just age ->
--           case noNegative w of
--             Nothing -> Nothing
--             Just weight ->
--               weightCheck (Cow name age weight)

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w = do
  name <- noEmpty n
  age <- noNegative a
  weight <- noNegative w
  weightCheck (Cow name age weight)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething :: Integer -> Maybe (Integer, Integer, String)
doSomething n = do
  a <- f n
  b <- g a
  c <- h b
  return (a, b, c)
