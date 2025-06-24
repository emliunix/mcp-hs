{-# LANGUAGE
  OverloadedStrings
, TypeApplications
, ScopedTypeVariables
#-}
module JsonRpc where

import Colog (WithLog, Message, logWarning)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Kind (Type)
import Data.String (fromString)
import GHC.Generics (Generic)

import FFree (FFree(..))
import Util (stripPrefixModifier)

data RpcRequest t = RpcRequest
    { requestJsonrpc :: String
    , requestId :: Maybe Int
    , requestMethod :: String
    , requestParams :: t
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

-- So per JSON-RPC's error hierarchy and it's need for returing error
-- we should split the parsing phases at request level and params level

-- mkHandler :: forall t r e m. (FromJSON t, ToJSON r, ToJSON e, MonadIO m) =>
--   (t -> (forall n. ToJSON n => n -> m ()) -> m (Either (RpcError e) r)) ->
--   (Value -> m ()) -> Maybe Int -> Value -> m ()
-- mkHandler f respond reqid req =
--   case fromJSON req of
--     Error err -> respondJson $ RpcResponse "2.0" reqid (Left $ RpcError InvalidParams err Nothing)
--     Success params -> do
--       f params $ \result -> do
--         case result of
--           Left err -> respondJson $ RpcResponse "2.0" reqid (Left err)
--           Right res -> respondJson $ RpcResponse "2.0" reqid (Right res)
--   where
--     respondJson :: RpcResponse e r -> m ()
--     respondJson = respond . toJSON

data Router (m :: Type -> Type) = MonadIO m => Router
  { handlers :: [(String, (Value -> m ()) -> Maybe Int -> Value -> m ())]
  }

-- | Do the RPC work
-- | taking a send function and a request object
-- | run on top of the Monad m of user's choice given it's MonadIO
rpc :: forall env m. (MonadIO m, WithLog env Message m) => Router m -> (Value -> m ()) -> Value -> m ()
rpc rt send req = do
    case fromJSON @(RpcRequest Value) req of
      Error err -> do
        logWarning $ "Invalid JSON or JSON-RPC request: " <> (fromString err)
        send . toJSON @(RpcResponse () ()) $
          RpcResponse "2.0" Nothing (Left $ RpcError InvalidRequest err Nothing)
      Success (RpcRequest _ reqid meth params) -> do
        case lookup_handler rt meth of
          Nothing -> send . toJSON @(RpcResponse () ()) $
            RpcResponse "2.0" reqid (Left $ RpcError MethodNotFound "Method not found" Nothing)
          Just f -> do
            f send reqid params
  where
    lookup_handler (Router hs) meth = lookup meth hs

-- data SessionStore = SessionStore
--   { sessions :: [(String, MVar Session)]
--   }

-- data Session m = Session
--   { sessionId :: String
--   , sessionRequestIdSeed :: Int
--   , sessionSend :: Value -> m ()
--   , sessionPendingClientResponses :: [(Int, Value -> IO ())]
--   }

-- data RpcT (m :: Type -> Type) a where
--   RpcPure :: a -> RpcT m a
--   RpcSend :: forall t r e. (ToJSON t, FromJSON r, FromJSON e) => (String, t) -> RpcT m (RpcResponse r e)
--   RpcNotify :: forall t. (ToJSON t) => t -> RpcT m ()

-- send :: (MonadIO m) => forall t r e. (ToJSON t, FromJSON r, FromJSON e) => String -> t -> RpcT m (Either String (RpcResponse e r))
-- send meth payload = RpcT $ do
--   modify $ \s -> s { sessionRequestIdSeed = sessionRequestIdSeed s + 1 }
--   rid <- sessionRequestIdSeed <$> get
--   let req = RpcRequest "2.0" (Just rid) meth payload
--   (sessionSend <$> get) <*> req
--   chan <- liftIO $ newChan
--   modify $ \s ->
--     s { sessionPendingClientResponses = (rid, \v -> writeChan chan) : sessionPendingClientResponses s }
--   res <- liftIO $ readChan chan
--   case fromJSON res of
--     Error err -> do
--       return . Left $ "Error in response: " <> (fromString err)
--     Success res -> do
--       return . Right $ res

-- notify :: (MonadIO m) => forall t. (ToJSON t) => Int -> t -> RpcT m ()
-- notify meth payload = RpcT $ do
--   let req = RpcRequest "2.0" Nothing meth payload
--   (sessionSend <$> get) <*> pure . toJSON $ req
--   return ()

data Rpc a where
  RpcSend :: (String, Value) -> Rpc (RpcResponse Value Value)
  RpcNotify :: (String, Value) -> Rpc ()

data Effects m a
  = Em (m a)
  | ERpc (Rpc a)

instance MonadIO m => MonadIO (FFree (Effects m)) where
  liftIO m = Impure (Em (liftIO m)) Pure

type FFreeEffects m a = FFree (Effects m) a

embed :: Monad m => m a -> FFree (Effects m) a
embed m = Impure (Em m) Pure

send :: String -> Value -> FFree (Effects m) (RpcResponse Value Value)
send meth req = Impure (ERpc (RpcSend (meth, req))) Pure

notify :: String -> Value -> FFree (Effects m) ()
notify meth req = Impure (ERpc (RpcNotify (meth, req))) Pure

runEffects :: Monad m => FFree (Effects m) a -> m a
runEffects fx = loop fx
  where 
    loop (Pure a) = pure a
    loop (Impure (Em m) k) = m >>= loop . k
    loop (Impure (ERpc rpc) k) = 
      case rpc of
        RpcSend (meth, req) -> do
          -- Here you would send the request and get the response
          let response = RpcResponse "2.0" Nothing (Right $ Object mempty) -- Placeholder for actual response
          loop (k response)
        RpcNotify (meth, req) -> do
          -- Here you would send the notification
          loop (k ())

type Handler m r = Monad m => (String, Maybe Int, Value) -> FFree (Effects m) (Either String r)

mkRequestHandler :: MonadIO m => forall t r e. (FromJSON t, ToJSON r, ToJSON e) =>
  (t -> FFree (Effects m) (Either (RpcError e) r)) -> Handler m Value
mkRequestHandler f (meth, reqid, req) = do
  return $ Left "not implemented yet"

-- mkNotificationHandler :: 