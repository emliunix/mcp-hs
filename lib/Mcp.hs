{-# LANGUAGE
  OverloadedStrings
#-}
module Mcp where

import Data.Aeson
import GHC.Generics (Generic)

import Util (stripPrefixModifier)

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
  { capabilitiesRoots :: Maybe McpCapability
  , capabilitiesLogging :: Maybe McpCapability
  , capabilitiesPrompts :: Maybe McpCapability
  , capabilitiesResources :: Maybe McpCapability
  , capabilitiesTools :: Maybe McpCapability
  , capabilitiesSampling :: Maybe McpCapability
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

instance FromJSON McpCapabilities where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "mcpCapabilities"
    }
  
data McpClientInfo = McpClientInfo
  { clientName :: String
  , clientVersion :: String
  } deriving (Show, Eq, Generic)

instance ToJSON McpClientInfo where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "client"
    }

instance FromJSON McpClientInfo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "client"
    }

data McpServerInfo = McpServerInfo
  { serverName :: String
  , serverVersion :: String
  } deriving (Show, Eq, Generic)

instance ToJSON McpServerInfo where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "server"
    }

instance FromJSON McpServerInfo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "server"
    }

data McpInitRequest = McpInitRequest
  { initRequestProtocolVersion :: String
  , initRequestCapabilities :: McpCapabilities
  , initRequestClientInfo :: McpClientInfo
  , initRequestInstructions :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON McpInitRequest where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "initRequest"
    }

instance FromJSON McpInitRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "initRequest"
    }

data McpInitResponse = McpInitResponse
  { initResponseProtocolVersion :: String
  , initResponseCapabilities :: McpCapabilities
  , initResponseServerInfo :: McpServerInfo
  , initResponseInstructions :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON McpInitResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "initResponse"
    }

instance FromJSON McpInitResponse where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefixModifier "initResponse"
    }

data Mcp = Mcp
  { mcpVersion :: String
  , mcpTools :: [McpTool]
  }

data McpTool = McpTool
  { toolName :: String
  , toolDescription :: String
  , toolInputSchema :: Value
  }
