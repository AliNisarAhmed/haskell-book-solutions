{-# LANGUAGE FlexibleInstances #-}

module JSONClass where

import           Control.Arrow (second)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject (JObj JValue)
  | JArray (JAry JValue)
  deriving (Ord, Eq, Show)


type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue b = JBool b
  fromJValue (JBool t) = Right t
  fromJValue _         = Left "Not a valid JBool value"


instance JSON String where
  toJValue s = JString s
  fromJValue (JString s) = Right s
  fromJValue _           = Left "Not a valid JString"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON Number"

newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Ord, Show)

newtype JObj a = JObj
  { fromJObj :: [(String, a)]
  } deriving (Eq, Ord, Show)

-- listToJValues :: (JSON a) => [a] -> [JValue]
-- listToJValues = map toJValue

-- jvaluesToJAry :: [JValue] -> JAry JValue
-- jvaluesToJAry = JAry

-- jaryOfJValuesToJValue :: JAry JValue -> JValue
-- jaryOfJValuesToJValue = JArray

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) =
  whenRight JAry (mapEithers fromJValue a)

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right $ f a

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) =
  case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of
      Left err -> Left err
      Right y  -> Right (y:ys)
mapEithers _ _ = Right []


instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where
      unwrap (k, v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not a JSON Object"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

-- instance (JSON a) => JSON [a] where
--   toJValue = undefined
--   fromJValue = undefined

-- instance (JSON a) => JSON [(String, a)] where
--   toJValue = undefined
--   fromJValue = undefined

