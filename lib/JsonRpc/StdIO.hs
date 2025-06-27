{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, GeneralizedNewtypeDeriving
, FunctionalDependencies
, ImpredicativeTypes
#-}
module JsonRpc.StdIO where

import Colog (WithLog, Message, LogAction, liftLogAction, usingLoggerT, logDebug, logInfo, logWarning, logError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.Error.Class (MonadError, catchError, throwError, modifyError)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Decoding.ByteString ()
import Data.String (fromString)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO (stdin, stdout)

import JsonRpc.NoJSON (pJSON)
import JsonRpc.Types
import JsonRpc.Rpc

import Util (fromJSON', note, noteM)

data NoJSONState = NoJSONState [B.ByteString] (B.ByteString -> AT.Result Int)

transport_stdio :: forall m. (MonadIO m, MonadError RpcErrors m) => LogAction m Message -> RpcRoutes m -> m ()
transport_stdio logAction handlers =
    loop initNoJson initSession $ \bs -> do
      logDebug $ "Received bytes: " <> fromString (show bs)
      val <- modifyError (\msg -> EInternal msg) . liftEither . A.eitherDecodeStrict @A.Value $ bs
      on_message handlers (liftIO . writeJson) val
  where
    initNoJson = NoJSONState [] (AT.parse pJSON)
    initSession = StdioSession 0 []
    loop :: NoJSONState -> StdioSession m -> (B.ByteString -> MonadStdioSessionT m ()) -> m ()
    loop state s handler = do
      input <- liftIO $ B.hGetSome stdin 4096
      if B.null input
        then return ()
        else do
          case feed state input of
            Left err -> usingLoggerT logAction $ logDebug $ "Error during parsing: " <> fromString err
            Right (chunks, state') -> do
              s' <- foldl (>>=) (pure s) (map (run . handler) chunks)
              loop state' s' handler
    run :: MonadStdioSessionT m r -> StdioSession m -> m (StdioSession m)
    run m s = do
      (_, s) <- flip runReaderT (liftLogAction logAction) . flip runStateT s . runMonadStdioSessionT $ m
      return s
    writeJson v = do
      LB.hPutStr stdout (A.encode v)
      B.hPutStr stdout "\n"

on_message :: 
  (Monad m, MonadError RpcErrors m, MonadIO m) =>
  RpcRoutes m ->
  (A.Value -> m ()) ->
  A.Value -> MonadStdioSessionT m ()
on_message handlers send value = do
  go `catchError` \err -> do
    case err of 
      ERpc rpcErr -> lift . send . A.toJSON @(RpcResponse A.Value ()) $ mkErrorResponse Nothing rpcErr
      ERpcForReq rpcErr reqId -> lift . send . A.toJSON @(RpcResponse A.Value ()) $ mkErrorResponse reqId rpcErr
      EInternal msg -> logError $ "RPC processing error: " <> fromString msg
  where
    go = do
      msg <- invalidRequest $ fromJSON' @IncomingMessage value
      logDebug $ "Received message: " <> fromString (show msg)
      case msg of
        IsRequest req -> onRequest req
        IsResponse res -> onResponse res
    onRequest req = do
      req <- invalidRequest $ fromJSON' @(RpcRequest A.Value) req
      logDebug $ "Processing request: " <> fromString (show req)
      handler <- methodNotFound (requestId req) $ note "handler not found" $ lookup_handler (requestMethod req) handlers
      logDebug $ "Found handler for method: " <> fromString (show (requestMethod req))
      loop (requestId req) $ handler (requestId req) (requestMethod req) (requestParams req)
    onResponse res = do
      res <- invalidRequest $ fromJSON' @(RpcResponse A.Value A.Value) res
      logDebug $ "Processing response: " <> fromString (show res)
      reqId <- note (EInternal "Response has no ID, ignoring") $ responseId res
      cont <- noteM (EInternal $ "can't resume to request " <> (show reqId)) $ takeCont reqId
      loop (Just reqId) $ cont (responseData res)
    loop reqId cont = do
      res <- lift . runRpcT $ cont
      case res of
        Done result -> do
          case result of
            Nothing -> return ()
            Just response -> do
              send' $ A.toJSON $ mkResponse @A.Value @A.Value reqId response
        DoRpc rpc k -> do
          logDebug $ "Requesting peer RPC call: " <> fromString (show rpc)
          case rpc of
            RpcSend (method, params) -> do
              reqId <- newRequestId
              saveCont reqId k
              send' $ A.toJSON $ mkRequest (Just reqId) method (Just params)
            RpcNotify (method, params) -> do
              send' $ A.toJSON $ mkRequest Nothing method (Just params)
              loop reqId $ k ()
    invalidRequest = modifyError $ \err -> ERpc $ RpcError InvalidRequest err Nothing
    methodNotFound reqid = modifyError $ \err -> ERpcForReq (RpcError MethodNotFound err Nothing) reqid
    send' = lift . send

-- | Feed a ByteString to the state and get the completed JSON chunks and new state
feed :: NoJSONState -> B.ByteString -> Either String ([B.ByteString], NoJSONState)
feed (NoJSONState chunks cont) input =
  loop1 (input : chunks) cont input []
  where
    loop1 chunks cont input output =
      case cont input of
        AT.Done remain sz -> let (bs, chunks') = bytes chunks sz in
          loop1 chunks' (AT.parse pJSON) remain (bs : output)
        AT.Partial cont' ->
          Right (reverse output, NoJSONState chunks cont')
        AT.Fail _ _ err -> Left err

-- | Take n bytes out of the accumulated bytes list
bytes :: [B.ByteString] -> Int -> (B.ByteString, [B.ByteString])
bytes [] _ = (B.empty, [])
bytes xs n =
  let (taken, remain, _) = foldr go ([], [], n) xs in
    (B.concat $ reverse taken, remain)
  where
    go x (taken, ys, 0) = (taken, x:ys, 0)
    go x (taken, ys, n') =
      if B.length x <= n'
        then (x : taken, ys, n' - B.length x)
        else (B.take n' x : taken, B.drop n' x : ys, 0)

type Cont m = (Either (RpcError A.Value) A.Value -> RpcT m (Maybe A.Value))

data StdioSession m = StdioSession
  { requestIdSeed :: Int
  , pendings :: [(Int, Cont m)]
  }

newtype MonadStdioSessionT m a =  MonadStdioSessionT
  { runMonadStdioSessionT :: StateT (StdioSession m)
      (ReaderT (LogAction (MonadStdioSessionT m) Message) m) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState (StdioSession m)
    , MonadReader (LogAction (MonadStdioSessionT m) Message)
    )

deriving instance (MonadError RpcErrors m) => MonadError RpcErrors (MonadStdioSessionT m)

newRequestId :: Monad m => MonadStdioSessionT m Int
newRequestId = do
  modify (\s -> s { requestIdSeed = requestIdSeed s + 1 })
  reqId <- requestIdSeed <$> get
  return reqId

takeCont :: Monad m => Int -> MonadStdioSessionT m (Maybe (Cont m))
takeCont reqId = do
  pendings' <- pendings <$> get
  case lookup reqId pendings' of
    Nothing -> return Nothing
    Just cont -> do
      modify (\s -> s { pendings = filter ((/= reqId) . fst) (pendings s) })
      return $ Just cont

saveCont :: Monad m => Int -> Cont m -> MonadStdioSessionT m ()
saveCont reqId cont = modify $ \s -> s { pendings = (reqId, cont) : pendings s }

instance MonadTrans MonadStdioSessionT where
  lift = MonadStdioSessionT . lift . lift

lookup_handler :: String -> RpcRoutes m -> Maybe (Handler m)
lookup_handler method (RpcRoutes routes) = lookup method routes
