module Connector where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.BERT
import Network.BERT.Transport

data Message
  = Position Float

instance BERT Message where
  showBERT msg = case msg of
    Position pos -> showBERT ("position", pos)

  readBERT t = do
    (tag, payload) <- readBERT t
    case tag of
      "position" -> Position <$> readBERT payload
      _ -> Left $ "Unexpected tag: " ++ tag

type PlayerID = String
type Position = Float

data Connector = Connector
  { sendMessage :: Message -> IO ()
  }

startConnector
  :: PlayerID -- ^ PlayerID
  -> MVar (Map PlayerID Position)
  -> IO Connector
startConnector playerId playersMap = do
  tcp <- tcpClient "localhost" 7777
  chan <- atomically newTChan
  let run = runSession tcp
  _ <- forkIO . run $ receiveThread playersMap
  _ <- forkIO . run $ sendThread playerId chan
  return $ Connector  $
    atomically . writeTChan chan

receiveThread
  :: MVar (Map PlayerID Position)
  -> TransportM ()
receiveThread playersMap = recvtForever $ \t ->
  case readBERT t of
    Left err -> liftIO $ putStrLn err
    Right (player, msg) ->
      case msg of
        Position pos -> liftIO $ modifyMVar_ playersMap $
          return . Map.insert player pos

sendThread
  :: PlayerID
  -> TChan Message
  -> TransportM ()
sendThread playerId msgChan = forever $ do
  msg <- liftIO . atomically $ readTChan msgChan
  sendt $ showBERT (playerId, msg)
