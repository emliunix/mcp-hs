module Util where

import Colog (WithLog, Message, logDebug, logInfo, logWarning, logError)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except (ExceptT, liftEither)
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

unPrefixOption :: String -> A.Options
unPrefixOption prefix = A.defaultOptions { A.fieldLabelModifier = stripPrefixModifier prefix }

fromJSON' :: (A.FromJSON a, Monad m) => A.Value -> ExceptT String m a
fromJSON' value = case A.fromJSON value of
  A.Success a -> return a
  A.Error err -> throwError $ "Failed to parse JSON: " <> fromString err

note :: e -> Maybe a -> Either e a
note err Nothing = Left err
note _ (Just x) = Right x

noteM :: MonadError e m => e -> Maybe a -> m a
noteM err a = liftEither . note err $ a

untilM :: Monad m => (a -> m Bool) -> m a -> m [a]
untilM p m = reverse <$> go []
  where
    go xs = do
      x <- m
      r <- p x
      if r then
        return $ xs
      else
        go (x : xs)

logDebug' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logDebug' msg = lift . logDebug $ msg

logInfo' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logInfo' msg = lift . logInfo $ msg

logWarning' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logWarning' msg = lift . logWarning $ msg

logError' :: (WithLog env Message m, Monad m, MonadTrans t) => Text -> t m ()
logError' msg = lift . logError $ msg
