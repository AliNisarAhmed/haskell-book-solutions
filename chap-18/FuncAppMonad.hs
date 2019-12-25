module FuncAppMonad where

import Data.List
import Data.Char
import Control.Applicative

-- main = do
--   line <- fmap reverse getLine
--   putStrLn $ "You said " ++ line ++ " in reverse!"

-- main = do
--   line <- fmap (intersperse '-' . reverse . map toUpper) getLine
--   putStrLn line

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines you typed were: " ++ a


-- instance Applicative ( (->) r ) where
--   pure x = \_ -> x
--   ( ((->) a1 f) <*> ((->) a1 x)) = (->) (a1 <> a2) (f x)

sequencea :: (Applicative f) => [f a] -> f [a]
-- sequencea [] = pure []
-- sequencea (x:xs) = (:) <$> x <*> sequenceA xs

sequencea = foldr (liftA2 (:)) (pure [])


---------------------------------------------------------

type Birds = Int
type Pole = (Birds, Birds)

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                   = Nothing

routine :: Maybe Pole
routine = do
  first <- return (0, 0)
  second <- landLeft 1 first
  -- Nothing
  third <- landRight 1 second
  landLeft 1 third

expr = return (0, 0) >>= landLeft 1 >>= landRight 1 >> Nothing >>= landLeft 1

