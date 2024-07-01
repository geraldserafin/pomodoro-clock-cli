{-# LANGUAGE ScopedTypeVariables #-}

module Deamon (start, sendMessage) where

import qualified Data.ByteString.Char8 as BS
import Commands
import Network.Socket
import Network.Socket.ByteString
import System.Posix hiding (Start)
import Data.Aeson
import Control.Exception
import Control.Monad
import GHC.IO.Exception
import Control.Concurrent
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Control.Monad.Extra

type MessageHandler = Maybe Message -> IO String

data State = State
  { notificationThread :: ThreadId
  , startTime          :: UTCTime
  , clockSettings      :: ClockSettings
  } deriving (Show)

path :: String
path = "/tmp/pomodoro_clock_cli_socket"

start :: IO ()
start = void . forkProcess $ do
  removeLink path `onException` return ()

  sock <- socket AF_UNIX Stream defaultProtocol

  bind   sock (SockAddrUnix path)
  listen sock 5

  state <- newMVar Nothing

  handleConnections sock $ createHandler state

handleConnections :: Socket -> MessageHandler -> IO ()
handleConnections sock handler = do
  (conn, _) <- accept sock
  msg       <- recv conn 1024

  let decodedMessage = decode (BS.fromStrict msg) :: Maybe Message

  res <- handler decodedMessage
  _   <- sendAll conn $ BS.pack res

  close conn
  handleConnections sock handler

sendMessage :: Message -> IO ()
sendMessage m =  do
  sock <- socket AF_UNIX Stream defaultProtocol

  connect sock (SockAddrUnix path) `onException` do
    putStrLn "Deamon is not running. Start it by using `pomodoro deamon start`."
    exitImmediately $ ExitFailure 1

  sendAll sock . BS.toStrict $ encode m

  res  <- recv sock 1024

  putStrLn $ BS.unpack res
  close sock

createHandler :: MVar (Maybe State) -> MessageHandler
createHandler _    Nothing  = return "Unknown command."
createHandler mvar (Just m) = response m
  where
    response (Start settings) = do
      time <- getCurrentTime
      
      modifyMVar_ mvar $ \maybeState -> do
        whenJust maybeState $ killThread . notificationThread 
        
        threadId <- forkIO $ do 
          threadDelay 1000000

        return . Just $ State threadId time settings
          
      return $ "Clock started with settings: " <> show settings

    response Status = do
      state <- readMVar mvar
      case state of
        Nothing -> return "Pomodoro clock is not running." 
        Just x  -> do
          currentTime <- getCurrentTime

          let (ClockSettings workT breakT cs) = clockSettings x
              elapsed   = round . toRational . nominalDiffTimeToSeconds . diffUTCTime currentTime $ startTime x
              cycleTime = (workT + breakT) * 60
              currentCycleTime  = cycleTime - elapsed `mod` cycleTime
              currentCycleState = if currentCycleTime >= breakT * 60 then "Work" else "Break"
              currentCycle = 1 + elapsed `div` cycleTime
              minutesLeft = currentCycleTime `div` 60
              secondsLeft = currentCycleTime `mod` 60

          return $ "Time left: " <> show minutesLeft <> ":" <> show secondsLeft <> "\n"
                <> "State: "     <> show currentCycleState  <> "\n"
                <> "Cycle: "     <> show currentCycle <> "/" <> show cs


