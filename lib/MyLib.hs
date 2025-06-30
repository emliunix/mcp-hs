{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , TypeFamilies
           , OverloadedStrings
#-}

module MyLib (runServer) where

import Colog
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Text.Blaze.Html (preEscapedToHtml)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Network.Wai.EventSource
import Network.HTTP.Types
import Yesod.Core
import Yesod.EventSource

import Mcp (mcpRoutes)
import JsonRpc hiding (Handler)
import JsonRpc.Wai

import TestApp
import TestMcpApp (helloTool)

data App = App
  { mcpApp :: Application
  }

mkYesod "App" [parseRoutes|
  / HomeR GET
  /api/v1/hello HelloR GET
  /api/v1/mcp McpR WaiSubsite mcpAPI
|]

instance Yesod App where

-- | return test html page from file index.html
getHomeR :: Handler Html
getHomeR = do
  html <- liftIO $ readFile "index.html"
  return $ preEscapedToHtml html

data HelloMsg = HelloMsg
  { message :: String
  } deriving (Show, Eq)

instance ToJSON HelloMsg where
    toJSON (HelloMsg msg) = object ["message" .= msg]

getHelloR :: Handler Value
getHelloR = do
    sendStatusJSON status200 (HelloMsg "Hello, World!")

mcpAPI :: App -> WaiSubsite
mcpAPI = WaiSubsite . mcpApp

runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  let tools = [helloTool]
  mcpApp <- transport_http appEnv logAct (mcpRoutes logAct tools)
  warp 3000 $ App { mcpApp = mcpApp }
  where
    logAct :: forall m. MonadIO m => LogAction m Message
    logAct = cmap fmtMessage logTextStderr
    appEnv = AppEnv 0 []
