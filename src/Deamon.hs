{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Deamon (start, sendMessage) where

import Commands
  ( ClockMessage (..)
  , ClockSettings (..)
  , Configuration (..)
  , HooksSettings (..)
  , StatusSettings (..)
  )
import Control.Concurrent
  ( MVar
  , forkIO
  , modifyMVar_
  , newMVar
  , readMVar
  , threadDelay
  )
import Control.Exception (SomeException, catch, try)
import Control.Monad (void)
import Control.Monad.Extra (when)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS
import Data.Hourglass (Elapsed (..))
import Data.List.Extra (replace)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (ExitCode (ExitSuccess))
import System.Hourglass (timeCurrent)
import System.Posix (exitImmediately, forkProcess, removeLink)
import System.Process.Extra (StdStream (NoStream), createProcess, proc, std_err)
import Text.Printf (printf)
import Utils (pairs, takeWhile_)

type MessageHandler = Maybe ClockMessage -> IO (Maybe String)

data State = State
  { breaks :: [Elapsed]
  , phaseTimes :: [Float]
  , config :: Configuration
  }

data PomodoroPhase = Work | ShortBreak | LongBreak deriving (Show, Eq)

data ClockStatus = ClockStatus
  { title :: Maybe String
  , timeRemainingInCurrentPhase :: Int
  , cyclesCompleted :: Int
  , cyclesGoal :: Int
  , phase :: PomodoroPhase
  }
  deriving (Show)

start :: Configuration -> String -> IO ()
start config path = void . forkProcess $ do
  removeLink path `catch` \(_ :: SomeException) -> return ()

  sock <- socket AF_UNIX Stream defaultProtocol

  bind sock (SockAddrUnix path)
  listen sock 5

  time <- timeCurrent
  state <-
    newMVar $
      State
        [time]
        (calculatePhaseTimes config.clockSettings)
        config

  void . forkIO $ triggerHook state

  handleConnections sock $ createHandler state

handleConnections :: Socket -> MessageHandler -> IO ()
handleConnections sock handler = do
  (conn, _) <- accept sock
  msg <- recv conn 1024

  let decodedMessage = decode (BS.fromStrict msg) :: Maybe ClockMessage

  res <- handler decodedMessage
  _ <- sendAll conn $ BS.toStrict $ encode res

  close conn
  handleConnections sock handler

sendMessage :: ClockMessage -> String -> IO (Maybe String)
sendMessage m path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  result <- try $ connect sock (SockAddrUnix path) :: IO (Either SomeException ())

  case result of
    Left _ -> return Nothing
    Right _ -> do
      sendAll sock . BS.toStrict $ encode m
      res <- recv sock 1024
      close sock
      return $ decode $ BS.fromStrict res

createHandler :: MVar State -> MessageHandler
createHandler _ Nothing = return Nothing
createHandler mvar (Just m) = response m
  where
    response Terminate = exitImmediately ExitSuccess
    response Toggle = do
      time <- timeCurrent
      modifyMVar_ mvar $ \state -> return state{breaks = time : state.breaks}
      return Nothing
    response (Status settings) = do
      state <- readMVar mvar
      time <- timeCurrent
      return . Just . formatStatus settings $ status time state

calculatePhaseTimes :: ClockSettings -> [Float]
calculatePhaseTimes cs =
  scanl1 (+) . map (* 60) . cycle . concat $
    replicate
      (cs.longBreakFrequency - 1)
      [cs.workTime, cs.shortBreakTime]
      ++ [[cs.workTime, cs.longBreakTime]]

status :: Elapsed -> State -> ClockStatus
status currentTime (State clockToggleTimes phaseTimes config) =
  let settings = config.clockSettings
      (Elapsed seconds) = sum . map (abs . uncurry (-)) . pairs $ clockToggleTimes <> [currentTime]
      elapsedSeconds = fromIntegral seconds :: Float
      completed = takeWhile_ (< elapsedSeconds) phaseTimes
      timeLeft = round $ last completed - elapsedSeconds
      currentCycle = (1 + length completed) `div` 2
      phase
        | odd . length $ completed = Work
        | length completed `mod` settings.longBreakFrequency == 0 = LongBreak
        | otherwise = ShortBreak
  in  ClockStatus settings.title timeLeft currentCycle settings.cycles phase

formatStatus :: StatusSettings -> ClockStatus -> String
formatStatus settings (ClockStatus tt t c g st) =
  foldr
    (uncurry replace)
    settings.format
    [ ("{time}", uncurry (printf "%02d:%02d") $ t `divMod` 60)
    , ("{state}", showPhase st)
    , ("{cycle}", show c)
    , ("{goal}", show g)
    , ("{title}", fromMaybe "" tt)
    ]
  where
    showPhase Work = settings.workText
    showPhase ShortBreak = settings.shortBreakText
    showPhase LongBreak = settings.longBreakText

runScript :: String -> IO ()
runScript filePath = void $ createProcess (proc "/bin/sh" [filePath]){std_err = NoStream}

triggerHook :: MVar State -> IO ()
triggerHook mvar = do
  threadDelay 1000000

  state <- readMVar mvar
  time <- timeCurrent

  let hooksSettings = state.config.hooksSettings
  let (ClockStatus _ t c g st) = status time state

  when (t == 0 && c == g && st == Work) $ do
    mapM_ runScript hooksSettings.onPomodoroEnd
    exitImmediately ExitSuccess

  when (t == 0 && st == Work) $ mapM_ runScript hooksSettings.onBreakStart
  when (t == 0 && st == ShortBreak) $ mapM_ runScript hooksSettings.onWorkStart

  triggerHook mvar
