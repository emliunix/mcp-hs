{-# LANGUAGE
  ScopedTypeVariables
, GADTs
#-}

module JsonRpc.Rpc where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Error.Class (MonadError(..), modifyError, throwError)

import qualified Data.Aeson as A

import JsonRpc.Types
import Util (fromJSON', note)

data Rpc a where
  RpcSend :: (String, A.Value) -> Rpc (Either (RpcError A.Value) A.Value)
  RpcNotify :: (String, A.Value) -> Rpc ()
deriving instance Show (Rpc a)

-- | To encode requiring a suspension for a peer RPC call.
data RpcF f a where
  Done :: a -> RpcF f a
  DoRpc :: (Rpc b) -> (b -> f a) -> RpcF f a

instance Functor f => Functor (RpcF f) where
  fmap f (Done a) = Done (f a)
  fmap f (DoRpc rpc cont) = DoRpc rpc (fmap (fmap f) cont)

newtype RpcT m a = RpcT
  { runRpcT :: m (RpcF (RpcT m) a)
  } deriving (Functor)

instance Monad m => Applicative (RpcT m) where
  pure = RpcT . pure . Done
  (<*>) = ap

instance Monad m => Monad (RpcT m) where
  a >>= k = RpcT $ do
    res <- runRpcT a
    case res of
      Done x -> runRpcT (k x)
      DoRpc rpc cont -> pure $ DoRpc rpc (fmap (>>= k) cont)

instance MonadTrans RpcT where
  lift m = RpcT (Done <$> m)
  
instance (MonadError RpcErrors m) => MonadError RpcErrors (RpcT m) where
  throwError err = RpcT $ throwError err
  catchError (RpcT m) handler = RpcT $ m `catchError` (runRpcT . handler)

instance MonadIO m => MonadIO (RpcT m) where
  liftIO io = RpcT (Done <$> liftIO io)

class MonadRpc m where
  rpcSend :: (String, A.Value) -> m (Either (RpcError A.Value) A.Value)
  rpcNotify :: (String, A.Value) -> m ()

instance Monad m => MonadRpc (RpcT m) where
  rpcSend (method, params) = RpcT $ pure (DoRpc (RpcSend (method, params)) (RpcT . pure . Done))
  rpcNotify (method, params) = RpcT $ pure (DoRpc (RpcNotify (method, params)) (RpcT . pure . Done))

-- | The type of handler for RPC methods
-- | for error response, throwError ERpc, which requires MonadError Error m
type Handler m = Maybe Int -> String -> A.Value -> RpcT m (Maybe A.Value)

newtype RpcRoutes m = RpcRoutes
  { unRpcRoutes :: [(String, Handler m)]
  }

lookup_handler :: String -> RpcRoutes m -> Maybe (Handler m)
lookup_handler method (RpcRoutes routes) = lookup method routes

mkRequestHandler :: (A.FromJSON t, A.ToJSON r, Monad m, MonadError RpcErrors m) => (Int -> String -> Maybe t -> RpcT m r) -> Handler m
mkRequestHandler f reqId method params = do
  reqId <- liftEither . note (ERpc (RpcError InvalidRequest "Missing request ID" Nothing)) $ reqId
  params <- invalidParams $ fromJSON' params
  res <- f reqId method params
  return . Just $ A.toJSON res
  where
    invalidParams = modifyError $ \err -> ERpcForReq (RpcError InvalidParams err Nothing) reqId

mkRequestHandler' :: (A.FromJSON t, A.ToJSON r, Monad m, MonadError RpcErrors m) => (Int -> String -> t -> RpcT m r) -> Handler m
mkRequestHandler' f reqId method params = mkRequestHandler f' reqId method params
  where
    f' reqId method params = do
      params' <- liftEither (note (ERpc (RpcError InvalidParams "Missing parameters" Nothing)) params)
      f reqId method params'

mkNotificationHandler :: (A.FromJSON t, Monad m, MonadError RpcErrors m) => (String -> Maybe t -> RpcT m ()) -> Handler m
mkNotificationHandler f _ method params = do
  params <- invalidParams $ fromJSON' params
  f method params
  return Nothing
  where
    invalidParams = modifyError $ \err -> ERpc (RpcError InvalidParams err Nothing)

rpcSend' :: (Monad m, MonadError RpcErrors m, MonadRpc m) => 
  forall t e r. (A.ToJSON t, A.FromJSON e, A.FromJSON r) =>
  String -> t -> m (Either (RpcError e) r)
rpcSend' method params = do
  let payload = A.toJSON params 
  res <- rpcSend (method, payload)
  case res of
    Left (RpcError code msg (Just err)) -> do
      err' <- liftEither . modifyError EInternal $ fromJSON' err
      return . Left $ RpcError code msg (Just err')
    Left (RpcError code msg Nothing) -> return . Left $ RpcError code msg Nothing
    Right res' -> do
      res'' <- liftEither . modifyError EInternal $ fromJSON' res'
      return . Right $ res''
