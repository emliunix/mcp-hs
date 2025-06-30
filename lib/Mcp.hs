{-# LANGUAGE
  OverloadedStrings
, DeriveAnyClass
#-}
module Mcp where

import Colog (LogAction, Message, logDebug, logError)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)

import qualified Data.Aeson as A

import Mcp.Types
import JsonRpc

data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , toolArgsSchema :: A.Value
  , toolFunc :: forall m. (MonadIO m, MonadError RpcErrors m) => A.Value -> AppT m (A.Value, Bool)
  }

mcpInitialize :: forall m. (Monad m, MonadError RpcErrors m) => Int -> String -> McpInitRequest -> AppT m McpInitResponse
mcpInitialize _ _ initReq = do
  logDebug $ "Received MCP initialization request: " <> fromString (show initReq)
  return $ McpInitResponse
    "2025-03-26"
    srvCaps
    srvInfo
    srvInstrs
  where
    srvInfo = McpServerInfo "test-mcp-server" "1.0"
    srvInstrs = Nothing
    srvCaps = McpCapabilities
      { mcpCapabilitiesRoots = Nothing
      , mcpCapabilitiesLogging = Nothing
      , mcpCapabilitiesPrompts = Nothing
      , mcpCapabilitiesResources = Nothing
      , mcpCapabilitiesTools = Just $ McpCapability
        { capabilityListChanged = False
        , capabilityList = True
        , capabilitySubscribe = False
        }
      , mcpCapabilitiesSampling = Nothing
      }

mcpNotifyInitialized :: forall m. (Monad m, MonadError RpcErrors m) => String -> () -> AppT m ()
mcpNotifyInitialized _ () = do
  logDebug "Received MCP notification: initialized"

mcpToolsList :: forall m. (Monad m, MonadError RpcErrors m) => [Tool] -> Int -> String -> McpToolsListRequest -> AppT m McpToolsListResponse
mcpToolsList tools _ _ (McpToolsListRequest cursor) = do
  logDebug $ "Received MCP tools list request: " <> fromString (show cursor)
  return $ McpToolsListResponse
    resTools
    Nothing
  where
    resTools = map
      (\(Tool name desc sch _) -> McpTool name desc sch)
      tools

mcpToolsCall :: forall m. (Monad m, MonadIO m, MonadError RpcErrors m) => [Tool] -> Int -> String -> McpToolsCallRequest -> AppT m McpToolsCallResponse
mcpToolsCall tools _ _ (McpToolsCallRequest name args) = do
  logDebug $ "Received MCP tools call request: " <> fromString (show (name, args))
  case lookupTool name tools of
    Nothing -> return $ McpToolsCallResponse (A.String "Tool not found") True
    Just tool -> do
      logDebug $ "Found tool: " <> fromString (show name)
      (result, isError) <- toolFunc tool args `catchError` \err -> do
        logError $ "Error executing tool: " <> fromString (show err)
        return (A.String $ "Error executing tool: " <> fromString (show err), True)
      return $ McpToolsCallResponse result isError
  where
    lookupTool n ts = case filter (\t -> toolName t == n) ts of
      [] -> Nothing
      (t:_) -> Just t

mcp_routes :: forall m. (Monad m, MonadIO m, MonadError RpcErrors m) => LogAction m Message -> [Tool] -> RpcRoutes m
mcp_routes logAct tools = RpcRoutes
  [ ("initialize", hRequest logAct mcpInitialize)
  , ("notifications/initialized", hNotification logAct mcpNotifyInitialized)
  , ("tools/list", hRequest logAct $ mcpToolsList tools)
  , ("tools/call", hRequest logAct $ mcpToolsCall tools)
  ]
