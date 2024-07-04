{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}

module Commands where

import Options.Applicative
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Command
  = Start   ClockSettings
  | Message ClockMessage

data ClockSettings = ClockSettings
  { workTime  :: Int
  , breakTime :: Int
  , cycles    :: Int
  } deriving (Show, Generic)

instance ToJSON ClockSettings
instance FromJSON ClockSettings

data ClockMessage = Terminate | Status deriving (Show, Generic)

instance ToJSON ClockMessage
instance FromJSON ClockMessage

startParser :: Parser Command
startParser = do
  wt <- option auto (long "work-time"  <> short 'w' <> value 25 <> metavar "INT" <> help "Time in minutes for work" )
  bt <- option auto (long "break-time" <> short 'b' <> value 5  <> metavar "INT" <> help "Time in minutes for break")
  cs <- option auto (long "cycles"     <> short 'c' <> value 1  <> metavar "INT" <> help "How many cycles to do"    )

  return $ Start $ ClockSettings wt bt cs

statusParser :: Parser Command
statusParser = pure $ Message Status

commandParser :: Parser Command
commandParser = subparser
  ( command "start"  (info (startParser  <**> helper) (progDesc "Start the timer"))
 <> command "status" (info (pure (Message Status   ) <**> helper) (progDesc "Show the status"))
 <> command "stop"   (info (pure (Message Terminate) <**> helper) (progDesc "Terminates the deamon"))
  )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "A simple timer application"
 <> header "timer - a simple timer application"
  )

parseCommand :: IO Command
parseCommand = execParser opts
