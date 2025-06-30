{-# LANGUAGE
  OverloadedStrings
#-}
module Mcp.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Util (unPrefixOption)

data McpCapability = McpCapability
  { capabilityListChanged :: Bool
  , capabilityList :: Bool
  , capabilitySubscribe :: Bool
  } deriving (Show, Eq)

_jsonTrue :: Value
_jsonTrue = Bool True

_jsonNull :: Value
_jsonNull = Null

instance ToJSON McpCapability where
  toJSON (McpCapability changed list subscribe) =
    object . filter  (\(_, v) -> v == _jsonTrue) $
    [ "listChanged" .= changed
    , "list" .= list
    , "subscribe" .= subscribe
    ]

instance FromJSON McpCapability where
  parseJSON = withObject "McpCapability" $ \o ->
    McpCapability
      <$> (isJustTrue <$> o .:? "listChanged")
      <*> (isJustTrue <$> o .:? "list")
      <*> (isJustTrue <$> o .:? "subscribe")
    where
      isJustTrue :: Maybe Bool -> Bool
      isJustTrue (Just True) = True
      isJustTrue _ = False

data McpSampling = McpSampling deriving (Show, Eq)

instance ToJSON McpSampling where
  toJSON _ = object []

instance FromJSON McpSampling where
  parseJSON = withObject "McpSampling" $ \_ -> pure McpSampling

data McpCapabilities = McpCapabilities
  { mcpCapabilitiesRoots :: Maybe McpCapability
  , mcpCapabilitiesLogging :: Maybe McpCapability
  , mcpCapabilitiesPrompts :: Maybe McpCapability
  , mcpCapabilitiesResources :: Maybe McpCapability
  , mcpCapabilitiesTools :: Maybe McpCapability
  , mcpCapabilitiesSampling :: Maybe McpCapability
  } deriving (Show, Eq, Generic)

instance ToJSON McpCapabilities where
  toJSON (McpCapabilities roots logging prompts resources tools sampling) =
    object . filter (\(_, v) -> v /= _jsonNull) $
    [ "roots" .= roots
    , "logging" .= logging
    , "prompts" .= prompts
    , "resources" .= resources
    , "tools" .= tools
    , "sampling" .= sampling
    ]

instance FromJSON McpCapabilities where parseJSON = genericParseJSON (unPrefixOption "mcpCapabilities")
  
data McpClientInfo = McpClientInfo
  { clientName :: String
  , clientVersion :: String
  } deriving (Show, Eq, Generic)

instance ToJSON McpClientInfo where toJSON = genericToJSON (unPrefixOption "client")
instance FromJSON McpClientInfo where parseJSON = genericParseJSON (unPrefixOption "client")

data McpServerInfo = McpServerInfo
  { serverName :: String
  , serverVersion :: String
  } deriving (Show, Eq, Generic)

instance ToJSON McpServerInfo where toJSON = genericToJSON (unPrefixOption "server")
instance FromJSON McpServerInfo where parseJSON = genericParseJSON (unPrefixOption "server")

data McpInitRequest = McpInitRequest
  { initRequestProtocolVersion :: String
  , initRequestCapabilities :: McpCapabilities
  , initRequestClientInfo :: McpClientInfo
  , initRequestInstructions :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON McpInitRequest where toJSON = genericToJSON (unPrefixOption "initRequest")
instance FromJSON McpInitRequest where parseJSON = genericParseJSON (unPrefixOption "initRequest")

data McpInitResponse = McpInitResponse
  { initResponseProtocolVersion :: String
  , initResponseCapabilities :: McpCapabilities
  , initResponseServerInfo :: McpServerInfo
  , initResponseInstructions :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON McpInitResponse where toJSON = genericToJSON (unPrefixOption "initResponse")
instance FromJSON McpInitResponse where parseJSON = genericParseJSON (unPrefixOption "initResponse")

data McpTool = McpTool
  { mcpToolName :: String
  , mcpToolTitle :: String
  , mcpToolDescription :: String
  , mcpToolInputSchema :: Value
  } deriving (Show, Generic)

instance ToJSON McpTool where toJSON = genericToJSON (unPrefixOption "mcpTool")
instance FromJSON McpTool where parseJSON = genericParseJSON (unPrefixOption "mcpTool")

data McpPaginationRequest = McpPaginationRequest
  { paginationRequestCursor :: Maybe String
  } deriving (Show, Generic)

instance ToJSON McpPaginationRequest where toJSON = genericToJSON (unPrefixOption "paginationRequest")
instance FromJSON McpPaginationRequest where parseJSON = genericParseJSON (unPrefixOption "paginationRequest")

data McpToolsListResponse = McpToolsListResponse
  { toolsListResponseTools :: [McpTool]
  , toolsListResponseNextCursor :: Maybe String
  } deriving (Show, Generic)

instance ToJSON McpToolsListResponse where
  toJSON (McpToolsListResponse tools nextCursor) =
    let props = [ "tools" .= tools ] in
      object $ case nextCursor of
        Nothing -> props
        Just cursor -> ("nextCursor" .= cursor) : props
instance FromJSON McpToolsListResponse where parseJSON = genericParseJSON (unPrefixOption "toolsListResponse")

data McpToolsCallRequest = McpToolsCallRequest
  { toolsCallRequestName :: String
  , toolsCallRequestArguments :: Value
  } deriving (Show, Generic)

instance ToJSON McpToolsCallRequest where toJSON = genericToJSON (unPrefixOption "toolsCallRequest")
instance FromJSON McpToolsCallRequest where parseJSON = genericParseJSON (unPrefixOption "toolsCallRequest")

data McpToolsCallResponse = McpToolsCallResponse
  { toolsCallResponseContent :: [ McpContent ]
  , toolsCallResponseIsError :: Bool
  } deriving (Show, Generic)

instance ToJSON McpToolsCallResponse where toJSON = genericToJSON (unPrefixOption "toolsCallResponse")
instance FromJSON McpToolsCallResponse where parseJSON = genericParseJSON (unPrefixOption "toolsCallResponse")

data McpContent
  = McpTextContent Text
  | McpBinaryContent String String String -- ^ (type, base64 encoded data, mime type)
  | McpResourceContent String String String -- ^ (uri, text, mime type)
  deriving (Show)

instance ToJSON McpContent where
  toJSON (McpTextContent text) =
    object ["type" .= ("text" :: Text), "text" .= text]
  toJSON (McpBinaryContent type' data' mimeType) =
    object ["type" .= type', "data" .= data', "mimeType" .= mimeType]
  toJSON (McpResourceContent uri text mimeType) =
    object ["type" .= ("resource" :: Text), "resource" .= object
      [ "uri" .= uri
      , "text" .= text
      , "mimeType" .= mimeType
      ]]
instance FromJSON McpContent where
  parseJSON = withObject "McpContent" $ \o -> do
    type' <- o .: "type"
    case type' of
      "text" -> McpTextContent <$> o .: "text"
      "binary" -> McpBinaryContent <$> o .: "data" <*> o .: "mimeType" <*> o .: "type"
      "resource" -> do
        resource <- o .: "resource"
        McpResourceContent <$> resource .: "uri" <*> resource .: "text" <*> resource .: "mimeType"
      _ -> fail $ "Unknown content type: " ++ show type'

data McpPrompt = McpPrompt
  { promptName :: String
  , promptDescription :: String
  , promptArguments :: [ McpPromptArgument ]
  } deriving (Show, Generic)

instance ToJSON McpPrompt where toJSON = genericToJSON (unPrefixOption "prompt")
instance FromJSON McpPrompt where parseJSON = genericParseJSON (unPrefixOption "prompt")

data McpPromptArgument = McpPromptArgument
  { promptArgumentName :: String
  , promptArgumentDescription :: String
  , promptArgumentRequired :: Bool
  } deriving (Show, Generic)

instance ToJSON McpPromptArgument where toJSON = genericToJSON (unPrefixOption "promptArgument")
instance FromJSON McpPromptArgument where parseJSON = genericParseJSON (unPrefixOption "promptArgument")

data McpPromptsListResponse = McpPromptsListResponse
  { promptsListResponsePrompts :: [McpPrompt]
  , promptsListResponseNextCursor :: Maybe String
  } deriving (Show, Generic)

instance ToJSON McpPromptsListResponse where
  toJSON (McpPromptsListResponse prompts nextCursor) =
    let props = [ "prompts" .= prompts ] in
      object $ case nextCursor of
        Nothing -> props
        Just cursor -> ("nextCursor" .= cursor) : props
instance FromJSON McpPromptsListResponse where parseJSON = genericParseJSON (unPrefixOption "promptsListResponse")
