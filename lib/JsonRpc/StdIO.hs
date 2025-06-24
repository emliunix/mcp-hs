{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module JsonRpc.StdIO where

import Colog (WithLog, Message, logDebug)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as A
import Data.String (fromString)
import JsonRpc.NoJSON (pJSON)
import qualified Data.Attoparsec.ByteString as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Aeson.Decoding.ByteString ()
import System.IO (stdin, stdout)

data State = State [B.ByteString] (B.ByteString -> AT.Result Int)

emptyState :: State
emptyState = State [] (AT.parse pJSON)

transport_stdio :: forall env m. (WithLog env Message m, MonadIO m) => ((A.Value -> m ()) -> A.Value -> m ()) -> m ()
transport_stdio handler = do
    loop emptyState $ \bs -> do
      logDebug $ "Received bytes: " <> fromString (show bs)
      case A.eitherDecodeStrict @A.Value bs of
        Left err -> logDebug $ "Failed to parse JSON: " <> fromString err
        Right val -> handler (\r -> liftIO $ LB.hPutStr stdout (A.encode r) >> B.hPutStr stdout "\n") val
  where
    loop :: State -> (B.ByteString -> m ()) -> m ()
    loop state handlerRaw = do
      input <- liftIO $ B.hGetSome stdin 4096
      if B.null input
        then return ()
        else do
          case feed state input of
            Left err -> logDebug $ "Error during parsing: " <> fromString err
            Right (chunks, state') -> do
              forM_ chunks handlerRaw
              loop state' handlerRaw

-- | Feed a ByteString to the state and get the completed JSON chunks and new state
feed :: State -> B.ByteString -> Either String ([B.ByteString], State)
feed (State chunks cont) input =
  loop1 (input : chunks) cont input []
  where
    loop1 chunks cont input output =
      case cont input of
        AT.Done remain sz -> let (bs, chunks') = bytes chunks sz in
          loop1 chunks' (AT.parse pJSON) remain (bs : output)
        AT.Partial cont' ->
          Right (reverse output, State chunks cont')
        AT.Fail _ _ err -> Left err

-- | Take n bytes out of the accumulated bytes list
bytes :: [B.ByteString] -> Int -> (B.ByteString, [B.ByteString])
bytes [] _ = (B.empty, [])
bytes xs n =
  let (taken, remain, _) = foldr go ([], [], n) xs in
    (B.concat $ reverse taken, remain)
  where
    go x (taken, ys, 0) = (taken, x:ys, 0)
    go x (taken, ys, n') =
      if B.length x <= n'
        then (x : taken, ys, n' - B.length x)
        else (B.take n' x : taken, B.drop n' x : ys, 0)
