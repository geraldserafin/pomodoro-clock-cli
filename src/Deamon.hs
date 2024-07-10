{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Deamon (start, sendMessage) where

import qualified Data.ByteString.Char8 as BS
import Commands ( ClockMessage(..), ClockSettings(ClockSettings) )
import Network.Socket
import Network.Socket.ByteString ( recv, sendAll )
import System.Posix ( removeLink, exitImmediately, forkProcess )
import Data.Aeson ( encode, decode )
import Control.Exception ( SomeException, try, catch )
import Control.Monad ( void )
import Control.Concurrent ( newMVar, readMVar, MVar, modifyMVar_, threadDelay, forkIO )
import System.Exit ( ExitCode (ExitSuccess) )
import Data.Hourglass ( Elapsed (..) )
import System.Hourglass ( timeCurrent )
import Utils ( pairs, takeWhile_ )
import Control.Monad.Extra (when)
import System.Process.Extra (callCommand)
import Data.List.Extra (replace)
import Text.Printf (printf)

type MessageHandler = Maybe ClockMessage -> IO (Maybe String)
data State = State { _time :: [Elapsed], _settings :: ClockSettings }
data ClockState = Work | Break deriving (Show, Eq)
data ClockStatus = ClockStatus
  { time    :: Int 
  , _cycle   :: Int
  , goal     :: Int
  , _state   :: ClockState
  } deriving (Show)

path :: String
path = "/tmp/pomodoro_clock_cli.sock"

start :: ClockSettings -> IO ()
start s = void . forkProcess $ do
  removeLink path `catch` \(_ :: SomeException) -> return ()

  sock <- socket AF_UNIX Stream defaultProtocol

  bind   sock (SockAddrUnix path)
  listen sock 5

  time  <- timeCurrent
  state <- newMVar $ State [time] s

  void . forkIO $ hookTrigger state

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

    response Toggle = do
      time <- timeCurrent
      modifyMVar_ mvar $ \state -> return state { _time = time : _time state }
      return Nothing

    response (Status fmt) = do
      state <- readMVar mvar
      time  <- timeCurrent
      return . Just . format fmt $ status time state

status :: Elapsed -> State -> ClockStatus
status ct (State st (ClockSettings wt sbt lbt cs lbf)) =
  let (Elapsed s)  = abs . sum . map (uncurry (-)) . pairs $ st <> [ct]
      elapsed      = fromIntegral s :: Int
      stages       = scanl1 (+) . map (*60) . cycle . concat $ replicate (lbf-1) [wt, sbt] ++ [[wt, lbt]]
      completed    = takeWhile_ (<elapsed) stages
      timeLeft     = last completed - elapsed
      state        = if odd $ length completed then Work else Break
      currentCycle = (1 + length completed) `div` 2
  in ClockStatus timeLeft currentCycle cs state

format :: String -> ClockStatus -> String
format fmt (ClockStatus t c g st) = foldr (uncurry replace) fmt 
  [ ("{time}" , uncurry (printf "%02d:%02d") $ t `divMod` 60)
  , ("{state}", show st)
  , ("{cycle}", show c )
  , ("{goal}" , show g )
  ]

hookTrigger :: MVar State -> IO ()
hookTrigger mvar = do
  threadDelay 1000000

  state <- readMVar mvar
  time  <- timeCurrent
  let (ClockStatus t c g st) = status time state

  when (t == 0 && c == g && st == Work) $ exitImmediately ExitSuccess

  when (t == 0 && st == Work)  $ callCommand "~/.pomodoro/on-break-start.sh 2>/dev/null"
  when (t == 0 && st == Break) $ callCommand "~/.pomodoro/on-work-start.sh  2>/dev/null"

  hookTrigger mvar
