module Contra where

import           Data.Functor.Contravariant

isTen :: Op Bool Int
isTen = Op (== 10)

isListOfTen :: Op Bool [a]
isListOfTen = contramap length isTen
-- Above, length is applied first, then isTen is applied.
-- so essentiallu contramap is flipped function composition
-- i.e. contramap = flip (.)  !!!! important intuition

-- trying to write the above functions using regualar functor

isTen2 :: Int -> Bool
isTen2 = (== 10)

isListOfTen2 :: [a] -> Bool
isListOfTen2 = isTen2 . length
-- OR
-- isListOfTen2 = fmap isTen length
-- so the intuition is that fmap is just a regular func composition.
-- and, the order of the functions in both fmap and (.) remains the same


-- Predicates
-- the above function, isTen, can also be written using Predicate

isTen3 :: Predicate Int
isTen3 = Predicate (== 10)

isListOfTen3 :: Predicate [a]
isListOfTen3 = contramap length isTen3
-- above, length will be applied first to the list input
-- then, isTen3 will be applied to the Int
-- we will use it like this
-- getPredicate isListOfTen3 [1..10] -> True
-- getPredicate takes two arguments
-- getPredicate ::
    -- Predicate a (this gives us the func a -> Bool) -> a -> Bool



---- Equivalence

-- (Equivalence a) is a just a function (a -> a -> Bool)
-- so it will take two arguments of type a


-- defaultEquivalence is (==) wrapped by Equivalence
-- getEquivalence defaultEquivalence 4 5 -> False
-- getEquivalence defaultEquivalence 5 5 -> True

-- we can define our own equivalence

fstEquivalence :: Eq a => Equivalence (a, b)
fstEquivalence = contramap fst defaultEquivalence
-- the above function is testing the first element of the two given
  -- tuples using (==), i.e. simple equality
-- getEquivalence fstEquivalence (1, 2) (1, 3) -> True


-- SImilarly
sndEquivalence :: Eq b => Equivalence (a, b)
sndEquivalence = contramap snd defaultEquivalence  -- snd applied first

-- getEquivalence sndEquivalence (1, 2) (1, 3) -> False
