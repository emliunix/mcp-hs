{-# LANGUAGE
  OverloadedStrings
, RequiredTypeArguments
#-}
module JsonSchema (
  JsonType(..)
, Props(..)
, schema
, isType
, isObject
, isArray
, hasTitle
, hasDescription
, hasRequired
) where

import qualified Data.Aeson as A (Value(..), object)
import Data.Aeson (KeyValue(..), ToJSON(..))
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import Data.Proxy (Proxy(..))

data JsonType
  = JsonNull
  | JsonString
  | JsonNumber
  | JsonBoolean
  | JsonArray
  | JsonObject

newtype Props (ts :: [JsonType]) = Props [(Text, A.Value)]

class TypesOf (ts :: [JsonType]) where
  typesOf :: Proxy ts -> [Text]

instance TypesOf '[] where typesOf _ = []
instance TypesOf ts => TypesOf ('JsonObject ': ts) where typesOf _ = "object" : typesOf (Proxy @ts)
instance TypesOf ts => TypesOf ('JsonArray ': ts) where typesOf _ = "array" : typesOf (Proxy @ts)
instance TypesOf ts => TypesOf ('JsonString ': ts) where typesOf _ = "string" : typesOf (Proxy @ts)
instance TypesOf ts => TypesOf ('JsonNumber ': ts) where typesOf _ = "number" : typesOf (Proxy @ts)
instance TypesOf ts => TypesOf ('JsonBoolean ': ts) where typesOf _ = "boolean" : typesOf (Proxy @ts)
instance TypesOf ts => TypesOf ('JsonNull ': ts) where typesOf _ = "null" : typesOf (Proxy @ts)

schema :: Props '[]
schema = Props []

hasTitle :: Props ts -> Text -> Props ts
hasTitle  (Props props) title = Props (("title", A.String title) : props)

hasDescription :: Props ts -> Text -> Props ts
hasDescription (Props props) desc = Props (("description", A.String desc) : props)

data Field where
  Field :: forall (ts :: [JsonType]). Text -> Props ts -> Field

newtype Fields = Fields [Field]
instance ToJSON Fields where
  toJSON (Fields fields) = A.object $ map (\(Field name (Props props)) -> (fromText name .= toJSON props)) fields

isObject :: Props ts -> [Field] -> Props (JsonObject ': ts)
isObject (Props props) fields = Props $ ("properties", toJSON $ Fields fields) : props

hasRequired :: Has 'JsonObject ts => Props ts -> [Text] -> Props ts
hasRequired (Props props) required = Props $ ("required", toJSON required) : props

class Has (t :: k) (ts :: [k]) where
instance Has t (t ': ts)
instance Has t ts => Has t (t' ': ts)

instance forall (ts :: [JsonType]). TypesOf ts => ToJSON (Props ts) where
  toJSON (Props @ts' props) = toObj $ ("types", toJSON $ typesOf (Proxy @ts')) : props

toObj :: [(Text, A.Value)] -> A.Value
toObj props = A.object . map (\(k, v) -> (fromText k) .= v) $ props

isArray :: TypesOf ts' => Props ts -> Props ts' -> Props (JsonArray ': ts)
isArray (Props ps1) ps2 =
  Props $ ("items", toJSON ps2) : ps1

isType :: forall (ts :: [JsonType]). Props ts -> forall (t :: JsonType) -> Props (t ': ts)
isType (Props props) _ = Props props
