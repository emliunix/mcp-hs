{-# LANGUAGE
  OverloadedStrings
, DeriveAnyClass
#-}
module Mcp where

import Colog (LogAction, Message, logDebug, logError)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS

import Mcp.Types
import JsonRpc

data Tool = Tool
  { toolName :: String
  , toolTitle :: String
  , toolDescription :: String
  , toolArgsSchema :: A.Value
  , toolFunc :: forall m. (MonadIO m, MonadError RpcErrors m) => A.Value -> AppT m ([McpContent], Bool)
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
    srvInstrs = Just "A test mcp server aims to provide tools"
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

mcpNotifyInitialized :: forall m. (Monad m, MonadError RpcErrors m) => String -> Maybe () -> AppT m ()
mcpNotifyInitialized _ _ = do
  logDebug "Received MCP notification: initialized"

mcpToolsList :: forall m. (Monad m, MonadError RpcErrors m) => [Tool] -> Int -> String -> Maybe McpPaginationRequest -> AppT m McpToolsListResponse
mcpToolsList tools _ _ params = do
  let cursor = case params of
        Just (McpPaginationRequest c) -> c
        Nothing -> Nothing
  logDebug $ "Received MCP tools list request, cursor: " <> fromString (show cursor)
  return $ McpToolsListResponse
    resTools
    Nothing
  where
    resTools = map
      (\(Tool name title desc sch _) -> McpTool name title desc sch)
      tools

mcpToolsCall :: forall m. (Monad m, MonadIO m, MonadError RpcErrors m) => [Tool] -> Int -> String -> McpToolsCallRequest -> AppT m McpToolsCallResponse
mcpToolsCall tools reqId _ (McpToolsCallRequest name args) = do
  logDebug $ "Received MCP tools call request: " <> fromString (show (name, args))
  case lookupTool name tools of
    Nothing -> throwError $ ERpcForReq (RpcError (ServerError (-32000)) "Tool not found" Nothing) (Just reqId)
    Just tool -> do
      logDebug $ "Found tool: " <> fromString (show name)
      (result, isError) <- toolFunc tool args `catchError` \err -> do
        logError $ "Error executing tool: " <> fromString (show err)
        return ([McpTextContent $ "Error executing tool: " <> fromString (show err)], True)
      return $ McpToolsCallResponse result isError
  where
    lookupTool n ts = case filter (\t -> toolName t == n) ts of
      [] -> Nothing
      (t:_) -> Just t

mcpPromptsList :: forall m. (Monad m, MonadError RpcErrors m) => Int -> String -> Maybe McpPaginationRequest -> AppT m McpPromptsListResponse
mcpPromptsList _ _ params = do
  let cursor = case params of
        Just (McpPaginationRequest c) -> c
        Nothing -> Nothing
  logDebug $ "Received MCP prompts list request, cursor: " <> fromString (show cursor)
  return $ McpPromptsListResponse
    [] -- Placeholder for prompts, as no prompts are defined in this example
    Nothing

mcpRoutes :: forall m. (Monad m, MonadIO m, MonadError RpcErrors m) => LogAction m Message -> [Tool] -> RpcRoutes m
mcpRoutes logAct tools = RpcRoutes
  [ ("initialize", hRequest' logAct mcpInitialize)
  , ("notifications/initialized", hNotification logAct mcpNotifyInitialized)
  , ("tools/list", hRequest logAct $ mcpToolsList tools)
  , ("tools/call", hRequest' logAct $ mcpToolsCall tools)
  , ("prompts/list", hRequest logAct $ mcpPromptsList)
  ]
