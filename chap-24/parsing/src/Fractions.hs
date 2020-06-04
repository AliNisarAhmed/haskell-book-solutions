{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- parseFull :: Parser Integer
-- parseFull = sliced .

main :: IO ()
main = do
  let pf = parseString parseFraction mempty
  print $ pf shouldWork
  print $ pf shouldAlsoWork

  print $ pf badFraction
  print $ pf alsoBad

---

type NumberOrFraction = Either Double Rational

parseNumberOrFraction :: Parser NumberOrFraction
parseNumberOrFraction =
  (Left <$> try double) <|> (Right <$> try parseFraction)
