{-# LANGUAGE
  OverloadedStrings
, FunctionalDependencies
#-}
module JsonRpc.Wai where

-- | MVar of requestIdSeed
-- | Chan of repsonse, actually Id -> Chan

import Colog (LogAction, HasLog(..), liftLogAction, Message, usingLoggerT, logDebug, logInfo, logWarning, logError)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Except (ExceptT(..), runExceptT, liftEither)
import Control.Monad.Error.Class (MonadError(..), modifyError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Aeson ((.=))
import Data.Maybe (fromMaybe)
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.String (fromString)
import Data.Text (Text)
import Network.Wai (Application, responseLBS, responseStream, getRequestBodyChunk, requestHeaders)
import Network.Wai.EventSource (eventSourceAppChan)
import Network.Wai.EventSource.EventStream (eventToBuilder, ServerEvent(..))
import Network.HTTP.Types (status200, status202, status400, status405, status500, hContentType)

import qualified Network.Wai as Wai
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import JsonRpc.Types
import JsonRpc.Rpc
import Util (note, untilM, fromJSON')

data AppEnv = AppEnv
  { requestIdSeed :: Int
  , requestPendings :: [(Int, Chan (Either (RpcError A.Value) A.Value))]
  }

transport_http :: AppEnv -> LogAction IO Message -> RpcRoutes (ExceptT RpcErrors IO) -> IO Application
transport_http env logAct routes = do
  env <- newMVar env
  return $ transport_http' env logAct routes

transport_http' :: MVar AppEnv -> LogAction IO Message -> RpcRoutes (ExceptT RpcErrors IO) -> Application
transport_http' env logAct rpcRoutes request respond = do
  let method = Wai.requestMethod request
  let contentType = lookup hContentType $ requestHeaders request
  usingLoggerT logAct $ logInfo $
    "Received HTTP request, method: " <> fromString (show method) <>
    " Content-Type: " <> fromString (show $ contentType)
  case (method, contentType) of
    ("POST", Just "application/json") -> do
      payload <- BS.concat <$> untilM (pure . BS.null) (getRequestBodyChunk request)
      res1 <- runExceptT @RpcErrors $ go payload
      case res1 of
        Left err -> do
          usingLoggerT logAct $ logError $ "Error processing request: " <> fromString (show err)
          case err of
            ERpc       err     -> respondJson status200 $ A.toJSON $ mkErrorResponse @A.Value @A.Value Nothing err
            ERpcForReq err rid -> respondJson status200 $ A.toJSON $ mkErrorResponse @A.Value @A.Value rid err
            EInternal  err     -> respondJson status500 $ A.object ["error" .= ("Internal server error: " <> (fromString err) :: Text)]
        Right response -> return response
    ("GET", _) -> respond $ responseLBS status405 [] LBS.empty -- dummySSE request respond
    _ -> respondJson status400 $ A.object ["error" .= ("Invalid request method or content type" :: Text)]
  where
    logActE = liftLogAction logAct
    go payload = do
      usingLoggerT logActE $ logDebug $ "Processing payload: " <> fromString (show payload)
      incoming <- invalidRequest . liftEither $ A.eitherDecodeStrict @IncomingMessage payload
      case incoming of
        IsResponse res -> goResponse res
        IsRequest req -> goRequest req
    goResponse res = do
      usingLoggerT logActE $ logDebug $ "Received response: " <> fromString (show res)
      res' <- invalidRequest $ fromJSON' @(RpcResponse A.Value A.Value) res
      resId <- invalidRequest . liftEither . note "missing responseId" $ responseId res'
      runHttpSession
        (SessionInner env
          (const $ pure ()) -- no needs for write here, only need to push response with env
          (liftLogAction logActE))
        (receiveRpcResponse resId (responseData res'))
      -- empty OK response for POST response
      liftIO respondOkEmpty
    goRequest req = do
      usingLoggerT logActE $ logDebug $ "Received request: " <> fromString (show req)
      req <- invalidRequest $ fromJSON' @(RpcRequest A.Value) req
      handler <- methodNotFound $ lookup_handler (requestMethod req) rpcRoutes
      let reqId = (requestId req)
      -- initiate processing
      res <- runRpcT $ handler reqId (requestMethod req) (fromMaybe A.Null $ requestParams req)
      case res of
        Done (Just res) -> liftIO . respondJson status200 $ A.toJSON $ mkResponse @A.Value @() reqId res
        Done Nothing -> liftIO respondOkEmpty
        DoRpc rpc k -> do
          usingLoggerT logActE $ logDebug $ "Requires to client RPC, Promoting to SSE: " <> fromString (show rpc)
          liftIO . respond $ responseStream -- promote to SSE
            status200
            [("Content-Type", "text/event-stream")]
            $ startSse reqId (RpcT $ pure (DoRpc rpc k))
    startSse reqId rpc write flush =
      let inner = SessionInner env (writeSseJson write flush) (liftLogAction logActE)
      in do
        _ <- runExceptT . runHttpSession inner
          $ catchError (doRpc reqId rpc)
          $ \err -> sseErr err
        return ()
    sseErr err = do
      logError $ "Error during SSE processing: " <> fromString (show err)
      case err of
        ERpc rpcErr             -> sendRpcResponse . A.toJSON $ mkErrorResponse @() @A.Value Nothing rpcErr
        ERpcForReq rpcErr reqId -> sendRpcResponse . A.toJSON $ mkErrorResponse @() @A.Value reqId rpcErr
        EInternal msg -> do
          logError $ "Internal error: " <> fromString msg
          sendRpcResponse . A.toJSON 
            $ mkErrorResponse @A.Value @A.Value Nothing 
            $ RpcError InternalError ("Internal server error: " <> msg) Nothing
    respondJson status data_ = respond $ responseLBS
      status
      [("Content-Type", "application/json")]
      (A.encode data_)
    -- | for response and notification
    respondOkEmpty = respond $ responseLBS status202 [] LBS.empty
    invalidRequest = modifyError $ \msg -> ERpc $ RpcError InvalidRequest ("Invalid request: " <> msg) Nothing
    methodNotFound = liftEither . note (ERpc $ RpcError MethodNotFound "Method not found" Nothing)

writeSseJson :: (Builder -> IO ()) -> IO () -> A.Value -> IO ()
writeSseJson write flush data_ = do
  write $ fromMaybe (lazyByteString "") $ eventToBuilder $ ServerEvent Nothing Nothing $ lines $ A.encode data_
  flush
  where
    lines bs = map (lazyByteString) $ LBS.split 10 bs  -- Split by newline character (ASCII 10)

data SessionInner m = SessionInner
  { env :: MVar AppEnv
  , write :: A.Value -> IO ()
  , logAction :: LogAction m Message
  }

instance Monad m => HasLog (SessionInner m) Message m where
  getLogAction = logAction
  overLogAction f inner = inner { logAction = f (logAction inner) }

newtype HttpSessionT m a = HttpSessionT
  { runHttpSessionT :: ReaderT (SessionInner (HttpSessionT m)) m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (SessionInner (HttpSessionT m))
    )

instance MonadTrans HttpSessionT where
  lift = HttpSessionT . lift

instance (Monad m, MonadError RpcErrors m) => MonadError RpcErrors (HttpSessionT m) where
  throwError = HttpSessionT . throwError
  (HttpSessionT action) `catchError` handler = HttpSessionT (action `catchError` (runHttpSessionT . handler))

runHttpSession :: Monad m => SessionInner (HttpSessionT m) -> HttpSessionT m a -> m a
runHttpSession inner = flip runReaderT inner . runHttpSessionT

receiveRpcResponse :: (Monad m, MonadIO m, MonadError RpcErrors m) =>
  Int -> (Either (RpcError A.Value) A.Value) -> HttpSessionT m ()
receiveRpcResponse rid res = do
  SessionInner env _ _ <- ask @(SessionInner _)
  chan <- liftEither . note (ERpc $ RpcError InvalidRequest "response not found" Nothing)
          =<< liftIO (takePending env rid)
  liftIO $ writeChan chan res
  where
    takePending varEnv rid = do
      env <- takeMVar varEnv
      let pendings = requestPendings env
      case lookup rid pendings of
        Nothing -> do
          putMVar varEnv env
          return Nothing
        Just chan -> do
          let pendings' = filter ((/= rid) . fst) pendings
          putMVar varEnv $ env { requestPendings = pendings' }
          return $ Just chan

sendRpcRequest :: (Monad m, MonadIO m) => String -> A.Value -> HttpSessionT m (Either (RpcError A.Value) A.Value)
sendRpcRequest method params = do
  SessionInner env write _ <- ask @(SessionInner _)
  rid <- liftIO (newRequestId env)
  liftIO . write . A.toJSON $ mkRequest (Just rid) method (Just params)
  chan <- liftIO newChan
  liftIO (putPending env rid chan)
  res <- liftIO (readChan chan)
  return res
  where
    newRequestId varEnv = do
      env <- takeMVar varEnv
      let rid = requestIdSeed env
      putMVar varEnv $ env { requestIdSeed = rid + 1}
      return rid
    putPending varEnv rid chan = do
      env <- takeMVar varEnv
      let pendings = (rid, chan) : requestPendings env
      putMVar varEnv $ env { requestPendings = pendings }

sendRpcNotification :: (Monad m, MonadIO m) => String -> A.Value -> HttpSessionT m ()
sendRpcNotification method params = do
  SessionInner _ write _ <- ask @(SessionInner _)
  liftIO . write . A.toJSON $ mkNotification method (Just params)
  return ()

sendRpcResponse :: (Monad m, MonadIO m) => A.Value -> HttpSessionT m ()
sendRpcResponse response = do
  SessionInner _ write _ <- ask @(SessionInner _)
  liftIO . write . A.toJSON $ response
  return ()

doRpc :: (Monad m, MonadIO m, MonadError RpcErrors m) => Maybe Int -> RpcT m (Maybe A.Value) -> HttpSessionT m ()
doRpc reqId rpc = do
   res <- lift . runRpcT $ rpc
   case res of
     Done Nothing -> return () -- final but nothing to return
     Done (Just a) -> sendRpcResponse . A.toJSON $ mkResponse @A.Value @() reqId a -- final response
     DoRpc (RpcSend (method, params)) k -> do
       rpcRes <- sendRpcRequest method params
       doRpc reqId $ k rpcRes
     DoRpc (RpcNotify (method, params)) k -> do
       sendRpcNotification method params
       doRpc reqId $ k ()

dummySSE :: Application
dummySSE request respond = do
  chan <- newChan
  eventSourceAppChan chan request respond
