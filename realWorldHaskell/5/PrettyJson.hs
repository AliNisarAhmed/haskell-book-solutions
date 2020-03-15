module PrettyJSON (renderJValue) where

import           Prelude    hiding ((<>))
import           Prettify
import           SimpleJSON (JValue (..))

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray arr) = series '[' ']' renderJValue arr
renderJValue (JObject o) = series '{' '}' field o
  where
    field (name, value) =
      string name <> text ": " <> renderJValue value


value = renderJValue (JObject [("f", JNumber 1), ("q", JBool True)])
value2 = empty </> char 'a'
