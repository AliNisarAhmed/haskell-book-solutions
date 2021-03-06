module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup is in Prelude

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z'<*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

main :: IO ()
main = do
  -- print $ sequenceA [Just 3, Just 4, Just 5]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ( (,) <$> xs <*> ys )
  -- print $ fmap summed ( (,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  -- print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True (seqA 3)
  print $ foldr (&&) True (seqA 4)
  print $ seqA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)

  -- sequenceA :: (Applicative f, Foldable t) => t (f a) -> f (t a)
  -- so on #49
  -- f :: (->) a, t ::[]
  -- the output would be
  -- (->) [3, 8, even] 7

seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)