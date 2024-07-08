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
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Extra (callCommand)

type MessageHandler = Maybe ClockMessage -> IO (Maybe String)

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

  time  <- getCurrentTime
  state <- newMVar $ State time s
  
  _ <- forkIO $ do 
    scheduleHooks s
    exitImmediately ExitSuccess 

  handleConnections sock $ createHandler state

handleConnections :: Socket -> MessageHandler -> IO ()
handleConnections sock handler = do
  (conn, _) <- accept sock
  msg       <- recv conn 1024

  let decodedMessage = decode (BS.fromStrict msg) :: Maybe ClockMessage

  res <- handler decodedMessage
  _   <- sendAll conn $ BS.toStrict $ encode res

  close conn
  handleConnections sock handler

sendMessage :: ClockMessage -> IO (Maybe String)
sendMessage m =  do
  sock   <- socket AF_UNIX Stream defaultProtocol
  result <- try $ connect sock (SockAddrUnix path) :: IO (Either SomeException ())

  case result of
    Left  _ -> return Nothing 
    Right _ -> do
      sendAll sock . BS.toStrict $ encode m
      res <- recv sock 1024
      close sock
      return $ decode $ BS.fromStrict res

createHandler :: MVar State -> MessageHandler
createHandler _    Nothing  = return Nothing
createHandler mvar (Just m) = response m
  where
    response Terminate = exitImmediately ExitSuccess 
    response Status    = do
      state <- readMVar mvar
      return . formatPomodoro state <$> getCurrentTime

formatPomodoro :: State -> UTCTime -> String
formatPomodoro (State st (ClockSettings wt sbt lbt cs)) ct =
  let elapsedTimeInSeconds = round . toRational . nominalDiffTimeToSeconds $ diffUTCTime ct st
      cycleDurationInSeconds = (wt + sbt) * 60
      (completedCycles, elapsedTimeInCurrentCycle) = elapsedTimeInSeconds `divMod` cycleDurationInSeconds
      isWorkPeriod = elapsedTimeInCurrentCycle < wt * 60
      remainingTimeInCurrentPeriod = if isWorkPeriod
                                     then wt * 60 - elapsedTimeInCurrentCycle
                                     else cycleDurationInSeconds - elapsedTimeInCurrentCycle
      (minutesLeft, secondsLeft) = remainingTimeInCurrentPeriod `divMod` 60
      currentCycleState = if isWorkPeriod then "Work" else "Break"
  in printf "%s - %02d:%02d, %d/%d" currentCycleState minutesLeft secondsLeft (completedCycles + 1) cs

scheduleHooks :: ClockSettings -> IO ()
scheduleHooks (ClockSettings wt bt lbt cs) = replicateM_ cs $ do
  _ <- try $ callCommand "~/.pomodoro/on-work-start.sh 2>/dev/null" :: IO (Either SomeException ())
  threadDelay $ 1000000 * 60 * wt
  _ <- try $ callCommand "~/.pomodoro/on-break-start.sh 2>/dev/null" :: IO (Either SomeException ())
  threadDelay $ 1000000 * 60 * bt

