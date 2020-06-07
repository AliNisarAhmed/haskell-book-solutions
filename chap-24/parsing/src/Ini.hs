{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

-- INI File headers

headerEx :: ByteString
headerEx = "[blah]"

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair ( Header <$> some letter)


--- INI File Assignments

assignmentExample :: ByteString
assignmentExample = "woot=1"

assignmentExample2 :: ByteString
assignmentExample2 = "woot=1\n\nkey=value"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  key <- some letter
  _ <- char '='
  value <- some (noneOf "\n")
  skipEOL
  return (key, value)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


--- INI File Comments

commentsExample1 :: ByteString
commentsExample1 = "; last modified 1 April \
  \ 2001 by John Doe"

commentsExample2 :: ByteString
commentsExample2 = "; blah\n; woot\n \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany (
    do
      _ <- char ';' <|> char '#'
      skipMany (noneOf "\n")
      skipEOL
  )

--- INI File Sections

sectionExample1 :: ByteString
sectionExample1 = ";ignore me\n[states]\nChris=Texas"

sectionExample2 :: ByteString
sectionExample2 = [r|
; ignore me
[states]
Chris=Texas
|]

sectionExample3 :: ByteString
sectionExample3 = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section
  = Section Header Assignments
  deriving (Eq, Show)

newtype Config
  = Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhiteSpaces :: Parser ()
skipWhiteSpaces =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhiteSpaces
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

---
