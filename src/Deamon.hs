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
import GHC.IO.Exception
import Control.Concurrent
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Control.Monad.Extra
import Text.Printf
import System.Exit (exitSuccess)

type MessageHandler = Maybe Message -> IO String

data State = State
  { hooksThread   :: ThreadId
  , startTime     :: UTCTime
  , clockSettings :: ClockSettings
  } deriving (Show)

path :: String
path = "/tmp/pomodoro_clock_cli.sock"

start :: IO ()
start = void . forkProcess $ do
  removeLink path `catch` \(_ :: SomeException) -> return ()

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
    response Terminate = exitSuccess 

    response (Start settings) = do
      time <- getCurrentTime

      modifyMVar_ mvar $ \maybeState -> do
        whenJust maybeState $ killThread . hooksThread

        threadId <- forkIO $ do
          threadDelay 1000000

        return . Just $ State threadId time settings

      return $ "Clock started with settings: " <> show settings

    response Status = do
      maybeState <- readMVar mvar
      case maybeState of
        Nothing    -> return "Pomodoro clock is not running."
        Just state -> formatPomodoro state <$> getCurrentTime


formatPomodoro :: State -> UTCTime -> String
formatPomodoro (State _ startTime (ClockSettings workDuration breakDuration totalCycles)) currentTime =
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

