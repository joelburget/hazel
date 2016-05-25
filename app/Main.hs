module Main where

import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import qualified Network.WebSockets as WS

import Hazel

newtype ServerState = ServerState Computation

newServerState :: ServerState
newServerState = ServerState (FVar "fill me in")

main :: IO ()
main = do
  stateVar <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ \pending -> do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    talk conn stateVar

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn stateVar = do
  ServerState program <- readMVar stateVar
  WS.sendTextData conn (Ae.encode program :: BS.ByteString)
  -- msg <- WS.receiveData conn
  return ()
