{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Main where

import Colog (usingLoggerT, WithLog, Message, LogAction, logTextStderr, cmap, fmtMessage, logDebug, logInfo, logWarning, logError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import Data.Aeson (genericParseJSON, genericToJSON, FromJSON, ToJSON, defaultOptions)
import Data.String (fromString)
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)

import JsonRpc.StdIO (transport_stdio)
import JsonRpc (RpcErrors, RpcError, RpcT, RpcRoutes(..), mkRequestHandler, MonadRpc(..))
import JsonRpc.AppT (AppT, runApp)
import Util (logInfo', logDebug')

data MyMethodParams = MyMethodParams
  { param1 :: Text
  , param2 :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data MyMethodResponse = MyMethodResponse
  { result :: Text
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data MyError = MyError Text deriving (Show, Generic, A.ToJSON, A.FromJSON)

data TestClientMethodParams = TestClientMethodParams
  { testParam1 :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data TestClientMethodResult = TestClientMethodResult
  { testResult :: String
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

myMethod ::
  (MonadIO m, MonadError RpcErrors m) =>
  Int -> String -> MyMethodParams -> AppT m MyMethodResponse
myMethod reqId method params = do
  logDebug $ "Received params: " <> fromString (show params)
  res <- rpcSend ("client_method", A.toJSON (TestClientMethodParams 6))
  _ <- case res of 
    Left err -> logError (fromString (show err))
    Right result -> logInfo (fromString (show result))
  return $ MyMethodResponse "Success"

main :: IO ()
main = do
  _ <- runExceptT $ do
    transport_stdio logAct (RpcRoutes
      [ ("myMethod", hRequest myMethod)
      ])
  return ()
  where
    logAct = cmap fmtMessage logTextStderr
    hRequest reqHandler =
      mkRequestHandler $ \rid meth params -> runApp logAct $ reqHandler rid meth params
