{-# LANGUAGE
  GeneralizedNewtypeDeriving
#-}
module JsonRpc.AppT where

import Colog (LogAction, Message, liftLogAction)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))

import JsonRpc.Rpc
import JsonRpc.Types (RpcErrors)

newtype AppT m a = AppT
  { runAppT :: ReaderT (LogAction (AppT m) Message) (RpcT m) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (LogAction (AppT m) Message)
    )

instance MonadTrans AppT where
  lift = AppT . lift . lift

instance MonadError RpcErrors m => MonadError RpcErrors (AppT m) where
  throwError = AppT . throwError
  catchError (AppT action) handler = AppT (catchError action (runAppT . handler))

instance (Monad m) => MonadRpc (AppT m) where
  rpcSend = AppT . lift . rpcSend
  rpcNotify = AppT . lift . rpcNotify

runApp :: Monad m => LogAction m Message -> AppT m a -> RpcT m a
runApp logAction app = runReaderT (runAppT app) (liftLogAction logAction)