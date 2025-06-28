{-# LANGUAGE UndecidableInstances #-}
module Util.CologErr where

import Colog (LoggerT(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Error.Class (MonadError(..))

instance (Monad m, MonadError e m) => MonadError e (LoggerT msg m) where
  throwError = lift . throwError
  catchError action handler = LoggerT $ catchError (runLoggerT action) (runLoggerT . handler)
