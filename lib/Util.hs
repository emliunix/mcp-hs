module Util where

import Colog (WithLog, Message, logDebug, logInfo, logWarning, logError)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except (ExceptT)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Aeson as A

stripPrefixModifier :: String -> String -> String
stripPrefixModifier p s =
  if take (length p) s == p
  then uncap $ drop (length p) s
  else s
  where uncap s = (toLower . head) s : tail s

fromJSON' :: (A.FromJSON a, Monad m) => A.Value -> ExceptT String m a
fromJSON' value = case A.fromJSON value of
  A.Success a -> return a
  A.Error err -> throwError $ "Failed to parse JSON: " <> fromString err

note :: MonadError e m => e -> Maybe a -> m a
note err Nothing = throwError err
note _ (Just x) = return x

noteM :: MonadError e m => e -> m (Maybe a) -> m a
noteM err ma = do
  mx <- ma
  case mx of
    Nothing -> throwError err
    Just x -> return x

logDebug' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logDebug' msg = lift . logDebug $ msg

logInfo' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logInfo' msg = lift . logInfo $ msg

logWarning' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logWarning' msg = lift . logWarning $ msg

logError' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logError' msg = lift . logError $ msg
