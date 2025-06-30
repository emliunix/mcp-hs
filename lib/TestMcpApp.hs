{-# LANGUAGE
  OverloadedStrings
, DeriveGeneric
, DeriveAnyClass
#-}
module TestMcpApp where

import Colog (LogAction, Message, logDebug, logError)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import GHC.Generics (Generic)

import qualified Data.Aeson as A

import JsonRpc.AppT
import JsonRpc.Types (RpcErrors(..))
import Mcp (Tool(..))
import JsonSchema

helloToolSchema :: A.Value
helloToolSchema =
  A.toJSON $ schema `isObject` 
    [ Field "name" $ schema `isType` JsonString `hasDescription` "Name of whom you want to greet"
    , Field "message" $ schema `isType` JsonString `hasDescription` "Message to greet with"
    ]
    `hasRequired` ["name", "message"]
    `hasDescription` "Tool to greet someone"

data HelloArgs = HelloArgs
  { name :: String
  , message :: String
  } deriving (Show, Generic, A.FromJSON)

helloTool :: Tool
helloTool = Tool
  { toolName = "hello"
  , toolDescription = "A tool to greet someone"
  , toolArgsSchema = helloToolSchema
  , toolFunc = helloToolCall
  }

helloToolCall :: (Monad m, MonadError RpcErrors m) => A.Value -> AppT m (A.Value, Bool)
helloToolCall args = do
  logDebug $ "Executing hello tool with args: " <> fromString (show args)
  case A.fromJSON args of
    A.Error err -> throwError $ EInternal $ "Invalid arguments for hello tool: " <> err
    A.Success (HelloArgs name message) -> do
      let response = A.object ["greeting" A..= ("Hello, " <> name <> "! " <> message)]
      return (response, False)
