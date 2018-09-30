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
  , getNewMissiles :: IO [(PlayerID, XPosition)]
  }

startConnector
  :: PlayerID -- ^ PlayerID
  -- ^ Player position map
  -> MVar (Map PlayerID XPosition)
  -> IO Connector
startConnector playerId playersMap = do
  tcp <- tcpClient "localhost" 7777
  msgChan <- atomically newTChan
  missilesChan <- atomically newTChan
  let run = runSession tcp
  _ <- forkIO . run $ receiveThread playersMap missilesChan
  _ <- forkIO . run $ sendThread playerId msgChan
  return $ Connector {
    sendMessage = atomically . writeTChan msgChan
  , getNewMissiles = atomically $ readAll missilesChan
  }

readAll
  :: TChan a
  -> STM [a]
readAll = readAll' []
  where
    readAll' xs chan = do
      mbX <- tryReadTChan chan
      case mbX of
        Nothing -> return xs
        Just x -> readAll' (x:xs) chan

receiveThread
  :: MVar (Map PlayerID XPosition)
  -> TChan (PlayerID, XPosition)
  -> TransportM ()
receiveThread playersMap missilesChan = recvtForever $ \t ->
  case readBERT t of
    Left err -> liftIO $ putStrLn err
    Right (player, msg) ->
      case msg of
        NewPosition pos -> liftIO $ modifyMVar_ playersMap $
          return . Map.insert player pos
        Missile pos -> liftIO $ atomically $
          writeTChan missilesChan (player, pos)

sendThread
  :: PlayerID
  -> TChan Message
  -> TransportM ()
sendThread playerId msgChan = forever $ do
  msg <- liftIO . atomically $ readTChan msgChan
  sendt $ showBERT (playerId, msg)
