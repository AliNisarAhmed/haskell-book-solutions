{-# LANGUAGE FlexibleInstances #-}

module JSONClass where

import SimpleJSON

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
  fromJValue _ = Left "Not a valid Boolean value"


instance JSON String where
  toJValue s = JString s
  fromJValue (JString s) = Right s
  fromJValue _ = Left "Not a String"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON Number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined