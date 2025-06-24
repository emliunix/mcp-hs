{-# LANGUAGE OverloadedStrings #-}
module Main where

import Colog (usingLoggerT, WithLog, Message, logDebug, logTextStderr, cmap, fmtMessage)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (genericParseJSON, genericToJSON, FromJSON, ToJSON, defaultOptions)
import Data.String (fromString)
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)

import JsonRpc.StdIO (transport_stdio)
import JsonRpc (rpc, Router(..), mkHandler, RpcError)

data MyMethodParams = MyMethodParams
  { param1 :: Text
  , param2 :: Int
  } deriving (Show, Generic)

instance FromJSON MyMethodParams where
  parseJSON = genericParseJSON defaultOptions

data MyMethodResponse = MyMethodResponse
  { result :: Text
  } deriving (Show, Generic)

instance ToJSON MyMethodResponse where
  toJSON = genericToJSON defaultOptions

data MyError = MyError Text deriving (Show, Generic)
instance ToJSON MyError where
  toJSON (MyError msg) = A.String msg

myMethod :: (WithLog env Message m, MonadIO m) => MyMethodParams -> ExceptT (RpcError MyError) m MyMethodResponse
myMethod params = do
  lift . logDebug $ "Received params: " <> fromString (show params)
  return $ MyMethodResponse "Success"

main :: IO ()
main =
  usingLoggerT (cmap fmtMessage logTextStderr) $ app
  where
    app = transport_stdio $ rpc (Router
      [ ("myMethod", mkHandler myMethod)
      ])
