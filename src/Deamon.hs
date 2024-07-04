{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Deamon (start, sendMessage) where

import qualified Data.ByteString.Char8 as BS
import Commands
import Network.Socket
import Network.Socket.ByteString
import System.Posix hiding (elapsedTime, Start)
import Data.Aeson
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Text.Printf
import System.Exit (exitSuccess)

type MessageHandler = Maybe ClockMessage -> IO String

data State = State
  { startTime     :: UTCTime
  , clockSettings :: ClockSettings
  } deriving (Show)

path :: String
path = "/tmp/pomodoro_clock_cli.sock"

start :: ClockSettings -> IO ()
start s = void . forkProcess $ do
  removeLink path `catch` \(_ :: SomeException) -> return ()

  sock <- socket AF_UNIX Stream defaultProtocol

  bind   sock (SockAddrUnix path)
  listen sock 5

  time     <- getCurrentTime
  state    <- newMVar $ State time s

  handleConnections sock $ createHandler state

handleConnections :: Socket -> MessageHandler -> IO ()
handleConnections sock handler = do
  (conn, _) <- accept sock
  msg       <- recv conn 1024

  let decodedMessage = decode (BS.fromStrict msg) :: Maybe ClockMessage

  res <- handler decodedMessage
  _   <- sendAll conn $ BS.pack res

  close conn
  handleConnections sock handler

sendMessage :: ClockMessage -> IO String
sendMessage m =  do
  sock   <- socket AF_UNIX Stream defaultProtocol
  result <- try $ connect sock (SockAddrUnix path) :: IO (Either SomeException ())

  case result of
    Left  _ -> return "No pomodoro clock running."
    Right _ -> do
      sendAll sock . BS.toStrict $ encode m
      res <- recv sock 1024
      close sock
      return $ BS.unpack res

createHandler :: MVar State -> MessageHandler
createHandler _    Nothing  = return "Unknown command."
createHandler mvar (Just m) = response m
  where
    response Terminate = exitSuccess 
    response Status = do
      state <- readMVar mvar
      formatPomodoro state <$> getCurrentTime

formatPomodoro :: State -> UTCTime -> String
formatPomodoro (State startTime (ClockSettings workDuration breakDuration totalCycles)) currentTime =
  let elapsedTimeInSeconds = round . toRational . nominalDiffTimeToSeconds $ diffUTCTime currentTime startTime
      cycleDurationInSeconds = (workDuration + breakDuration) * 60
      (completedCycles, elapsedTimeInCurrentCycle) = elapsedTimeInSeconds `divMod` cycleDurationInSeconds
      isWorkPeriod = elapsedTimeInCurrentCycle < workDuration * 60
      remainingTimeInCurrentPeriod = if isWorkPeriod
                                     then workDuration * 60 - elapsedTimeInCurrentCycle
                                     else cycleDurationInSeconds - elapsedTimeInCurrentCycle
      (minutesLeft, secondsLeft) = remainingTimeInCurrentPeriod `divMod` 60
      currentCycleState = if isWorkPeriod then "Work" else "Break"
  in printf "%s - %02d:%02d, %d/%d" currentCycleState minutesLeft secondsLeft (completedCycles + 1) totalCycles

