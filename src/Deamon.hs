{-# LANGUAGE ScopedTypeVariables #-}

module Deamon (start, message) where

import qualified Data.ByteString.Char8 as BS
import Commands
import Network.Socket
import Network.Socket.ByteString
import System.Posix
import Data.Aeson
import Control.Exception
import Control.Monad

path :: String
path = "/tmp/pomodoro_clock_cli_socket"

start :: IO ()
start = void . forkProcess $ do
  removeLink path `catch` \(_ :: IOError) -> return ()

  sock <- socket AF_UNIX Stream defaultProtocol

  bind   sock (SockAddrUnix path)
  listen sock 5

  handleConnections sock

handleConnections :: Socket -> IO ()
handleConnections sock = do
  (conn, _) <- accept sock
  msg       <- recv conn 1024

  let decodedMessage = decode (BS.fromStrict msg) :: Maybe Message

  res <- handleMessage decodedMessage
  _   <- sendAll conn $ BS.pack res

  close conn
  handleConnections sock

sendMessage :: Message -> IO ()
sendMessage m =  do
  sock <- socket AF_UNIX Stream defaultProtocol

  connect sock $ SockAddrUnix path
  sendAll sock . BS.toStrict $ encode m

  msg  <- recv sock 1024
  
  print msg
  close sock

handleMessage :: Maybe Message -> IO String
handleMessage m = return $ "Message received: " <> show m
