module Connector
  ( PlayerID
  , XPosition
  , Connector(sendMessage)
  , startConnector
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (decode)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.BERT
import Network.BERT.Transport

import Message

type PlayerID = String
type XPosition = Float

data Connector = Connector
  { sendMessage :: Message -> IO ()
  }

startConnector
  :: PlayerID -- ^ PlayerID
  -> MVar (Map PlayerID XPosition)
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
  :: MVar (Map PlayerID XPosition)
  -> TransportM ()
receiveThread playersMap = recvtForever $ \t ->
  case t of
    BinaryTerm t' ->
      case readBERT (decode t') of
        Left err -> liftIO $ putStrLn err
        Right (player, msg) ->
          case msg of
            NewPosition pos -> liftIO $ modifyMVar_ playersMap $
              return . Map.insert player (read pos)

sendThread
  :: PlayerID
  -> TChan Message
  -> TransportM ()
sendThread playerId msgChan = forever $ do
  msg <- liftIO . atomically $ readTChan msgChan
  sendt $ showBERT (playerId, msg)
