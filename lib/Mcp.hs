{-# LANGUAGE
  OverloadedStrings
#-}
module Mcp where

import Data.Aeson
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

data Mcp = Mcp
  { mcpVersion :: String
  , mcpTools :: [McpTool]
  } deriving (Show, Generic)

data McpTool = McpTool
  { toolName :: String
  , toolDescription :: String
  , toolInputSchema :: Value
  } deriving (Show, Generic)
