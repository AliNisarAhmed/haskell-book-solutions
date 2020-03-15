module Prettify where

import           Data.Bits  (shiftR, (.&.))
import           Data.Char  (ord)
import           Numeric    (showHex)
import           Prelude    hiding ((<>))
import           SimpleJSON

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

empty :: Doc
empty = Empty


-----------------------------------------------


-- CORE LIBRARY FUNCTIONS

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar


text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double num = text (show num)

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> x = x
x <> Empty = x
x <> y = Concat x y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)


--------------------------------------------------

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softLine <> y

softLine :: Doc
softLine = group line

group :: Doc -> Doc
group x = Union (flatten x) x


-- flatten replaces new line with spaces
-- this function is needed to count the actual number of
  -- characters taken up by the rendering so far
flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line         = Char ' '
flatten (Union x _)  = flatten x
flatten other        = other

oneChar :: Char -> Doc
oneChar c =
  case lookup c simpleEscapes of
    Just r -> text r
    Nothing
      | mustEscape c -> hexEscape c
      | otherwise -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x =
  text "\\u"
  <> text (replicate (4 - length h) '0')
  <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where d = ord c




----------------------------------------------


----------------------------------------------


-- Pretty print an array

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item =
  enclose open close . fsep . punctuate (char ',') . map item

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []       = []
punctuate _ [d]      = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

--------------------------------------------------


-- compact ugly rendering of JSON, for machines.
compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d:ds) =
      case d of
        Empty      -> transform ds
        Char c     -> c : transform ds
        Text s     -> s ++ transform ds
        Line       -> '\n' : transform ds
        Concat a b -> transform (a: b : ds)
        Union _ b  -> transform (b: ds)


-----------------------------------------------

-- pretty : for human readable JSON
-- Int is the max width of the line
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d:ds) =
      case d of
        Empty      -> best col ds
        Char c     -> c : best (col + 1) ds
        Text s     -> s ++ best (col + length s) ds
        Line       -> '\n' : best 0 ds
        Concat a b -> best col (a:b:ds)
        Union a b  -> nicest col (best col (a:ds)) (best col (b:ds))
    best _ _ = ""
    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where least = min width col

fits :: Int -> String -> Bool
fits w _         | w < 0 = False
fits w ""        = True
fits w ('\n': _) = True
fits w (c:cs)    = fits (w - 1) cs


-- value = empty </> char 'a'
-- -> Concat (Union (Char ' ') Line) (Char 'a')
-- -> Stack = Union (Char ' ') Line ||| Char 'a'
-- -> Union (Char ' ') Line
-- -> nicest 0 (best 0 [Char ' ', Char 'a']) (best 0 [Line, Char 'a'])
