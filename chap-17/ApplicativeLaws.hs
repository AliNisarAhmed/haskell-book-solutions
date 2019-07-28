module ApplicativeLaws where

f x = pure id <*> x

-- id [1..5]
-- fmap id [1..5]
-- pure id <*> [1..5]


-- 2.

-- Composing two functions and applying to w
g u v w = pure (.) <*> u <*> v <*> w

-- appying two fnctios to w
g' u v w = u <*> (v <*> w)

-- 3.

-- Homomorphism

-- pure f <*> pure x = pure (f x)

-- 4.
-- Interchange

-- u <*> pure y = pure ($ y) <*> u

-- ON THE LEFT
-- u is a function embedded in some structure. (lifted function)
-- y is just a value being lifted by pure.
-- ON THE RIGHT
-- ($ y) is: y waiting for a function, to which it itself will be applied as argument
-- pure ($ y) lifts the partial applied y
-- u is a lifted function

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b
mApply = (<*>)

myResult = mPure ($ 2) `mApply` Just (+2)