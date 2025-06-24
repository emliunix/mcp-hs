{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , TypeFamilies
           , OverloadedStrings
#-}

module MyLib (runServer) where

import Control.Concurrent (threadDelay)
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

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET
  /api/v1/hello HelloR GET
  /api/v1/sse SseR GET
  /api/v1/mcp McpR GET POST
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

getSseR :: Handler TypedContent
getSseR = do
    -- let sseHeaders = [ ("Content-Type", "text/event-stream")
    --                  , ("Cache-Control", "no-cache")
    --                  ]
    let msg = object ["message" .= (T.pack "Hello, SSE!")]
    let bs = LBS.toStrict . encode . toJSON $ msg
    ioToRepEventSource (10 :: Int) $ \_ s -> do
        if s == 0
        then return ([CloseEvent], s)
        else do
          threadDelay 1000000  -- 1 second delay
          let builder = BB.byteString bs
          return ([ServerEvent Nothing Nothing [builder]], s - 1)

getMcpR :: Handler ()
getMcpR = do
    -- Handle GET request for MCP
    sendResponseStatus status200 ("MCP GET response" :: String)

postMcpR :: Handler ()
postMcpR = do
    -- Handle POST request for MCP
    reqBody <- requireCheckJsonBody :: Handler Value
    let response = object ["status" .= ("MCP POST received" :: String), "data" .= reqBody]
    sendResponseStatus status200 response

runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  warp 3000 App
