{-# LANGUAGE OverloadedStrings #-}
module JsonRpc.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Generics (Generic)

import Util (stripPrefixModifier)

data RpcRequest t = RpcRequest
    { requestJsonrpc :: String
    , requestId :: Maybe Int
    , requestMethod :: String
    , requestParams :: Maybe t
    } deriving (Show, Generic)

data RpcResponse e t = RpcResponse
    { responseJsonrpc :: String
    , responseId :: Maybe Int
    , responseData :: Either (RpcError e) t
    } deriving (Show, Generic)

data RpcErrorCode
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError Int -- ^ among the range -32000 to -32099
  deriving (Show, Eq)

toErrorCode :: RpcErrorCode -> Int
toErrorCode ParseError = -32700
toErrorCode InvalidRequest = -32600
toErrorCode MethodNotFound = -32601
toErrorCode InvalidParams = -32602
toErrorCode InternalError = -32603
toErrorCode (ServerError code) = code

fromErrorCode :: Int -> RpcErrorCode
fromErrorCode (-32700) = ParseError
fromErrorCode (-32600) = InvalidRequest
fromErrorCode (-32601) = MethodNotFound
fromErrorCode (-32602) = InvalidParams
fromErrorCode (-32603) = InternalError
fromErrorCode code  = ServerError code

data RpcError e = RpcError
    { errorCode :: RpcErrorCode
    , errorMessage :: String
    , errorData :: Maybe e
    } deriving (Show, Eq)

instance (ToJSON t) => ToJSON (RpcRequest t) where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "request"
    }

instance (FromJSON t) => FromJSON (RpcRequest t) where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "request"
    }

instance (ToJSON e) => ToJSON (RpcError e) where
  toJSON (RpcError code msg d) = object 
    [ "code" .= toErrorCode code
    , "message" .= msg
    , "data" .= d
    ]

instance FromJSON e => FromJSON (RpcError e) where
  parseJSON = withObject "RpcError" $ \v -> do
    RpcError <$> (fromErrorCode <$> v .: "code")
      <*> v .: "message"
      <*> v .:? "data"

instance (ToJSON e, ToJSON r) => ToJSON (RpcResponse e r) where
  toJSON (RpcResponse jsonrpc reqid res) =
    case res of
      Left err -> object
        [ "jsonrpc" .= jsonrpc
        , "id" .= reqid
        , "error" .= err
        ]
      Right result -> object
        [ "jsonrpc" .= jsonrpc
        , "id" .= reqid
        , "result" .= result
        ]

instance (FromJSON e, FromJSON r) => FromJSON (RpcResponse e r) where
  parseJSON = withObject "RpcResponse" $ \v -> do
    jsonrpc <- v .: "jsonrpc"
    reqid <- v .:? "id"
    res <- (Right <$> v .: "result") <|> (Left <$> v .: "error")
    return $ RpcResponse jsonrpc reqid res

data IncomingMessage
  = IsRequest Value
  | IsResponse Value
  deriving (Show, Eq)

instance FromJSON IncomingMessage where
  parseJSON = withObject "IncomingMessage" $ \v -> do
    (m :: Maybe String) <- v .:? "method"
    case m of
      Just _ -> pure $ IsRequest (Object v)
      Nothing -> pure $ IsResponse (Object v)

data RpcErrors
  = ERpc (RpcError Value)
  | ERpcForReq (RpcError Value) (Maybe Int)
  | EInternal String
  deriving (Show)

parseRequest :: forall t. FromJSON t => Value -> Either (RpcError ()) (RpcRequest t)
parseRequest v = case fromJSON v of
  Success req -> if requestJsonrpc req /= "2.0"
    then Left $ RpcError InvalidRequest "Invalid JSON-RPC version" Nothing
    else Right req
  Error err -> Left $ RpcError InvalidRequest (show err) Nothing

toRequest :: forall t. ToJSON t => RpcRequest t -> Value
toRequest = toJSON

parseResponse :: forall e r. (FromJSON e, FromJSON r) => Value -> Either String (RpcResponse e r)
parseResponse v = case fromJSON v of
  Success res -> if responseJsonrpc res /= "2.0"
    then Left "Invalid JSON-RPC version"
    else Right res
  Error err -> Left err

toResponse :: forall e r. (ToJSON e, ToJSON r) => RpcResponse e r -> Value
toResponse = toJSON

mkRequest :: forall t. ToJSON t => Maybe Int -> String -> Maybe t -> RpcRequest t
mkRequest reqid method params = RpcRequest
  { requestJsonrpc = "2.0"
  , requestId = reqid
  , requestMethod = method
  , requestParams = params
  }

mkNotification :: forall t. ToJSON t => String -> Maybe t -> RpcRequest t
mkNotification method params = RpcRequest
  { requestJsonrpc = "2.0"
  , requestId = Nothing
  , requestMethod = method
  , requestParams = params
  }

mkErrorResponse :: forall r e. (ToJSON r, ToJSON e) => Maybe Int -> RpcError e -> RpcResponse e r
mkErrorResponse reqid err = RpcResponse
  { responseJsonrpc = "2.0"
  , responseId = reqid
  , responseData = Left err
  }

mkResponse :: forall r e. (ToJSON r, ToJSON e) => Maybe Int -> r -> RpcResponse e r
mkResponse reqid result = RpcResponse
  { responseJsonrpc = "2.0"
  , responseId = reqid
  , responseData = Right result
  }
