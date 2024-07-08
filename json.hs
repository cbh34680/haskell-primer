import qualified Data.List as L


data JValue = JNumber Double | JString String | JBool Bool | JNull | JObject [(String, JValue)] | JArray [JValue]

quote::String -> String
quote cs = '"' : cs ++ "\""

renderJValue::JValue -> String
renderJValue (JNumber x) = show x
renderJValue (JString x) = quote x
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject xs) = "{" ++ inn ++ "}"
    where
        inn = concat . L.intersperse ", " $ map step xs
        step (k, v) = quote k ++ ": " ++ renderJValue v

renderJValue (JArray xs) = "[" ++ inn ++ "]"
    where
        inn = concat . L.intersperse ", " $ map renderJValue xs


f = renderJValue $ JObject [("a", JNull), ("b", JNumber 1), ("c", JString "c")]
g = renderJValue $ JArray [JNull, JNumber 1, JString "c"]
