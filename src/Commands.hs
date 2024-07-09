{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Commands where

import Options.Applicative
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Command
  = Start   ClockSettings
  | Message ClockMessage

data ClockSettings = ClockSettings
  { workTime      :: Int
  , breakTime     :: Int
  , longBreakTime :: Int
  , cycles        :: Int
  , longBreakFrequency :: Int
  } deriving (Show, Generic)

data ClockMessage = Terminate | Status String | Toggle deriving (Show, Generic)

$(deriveJSON defaultOptions ''ClockMessage)
$(deriveJSON defaultOptions ''ClockSettings)

startParser :: Parser Command
startParser = do
  wt  <- option auto (long "work-time"        <> short 'w' <> value 25 <> metavar "INT" <> help "Time in minutes for work"       )
  sbt <- option auto (long "short-break-time" <> short 's' <> value 5  <> metavar "INT" <> help "Time in minutes for short break")
  lbt <- option auto (long "long-break-time"  <> short 'l' <> value 15 <> metavar "INT" <> help "Time in minutes for long break" )
  cs  <- option auto (long "cycles"           <> short 'c' <> value 1  <> metavar "INT" <> help "How many cycles to do"          )
  lbf <- option auto (long "long-break-freq"  <> short 'f' <> value 4  <> metavar "INT" <> help "Every nth cycle to have a longer break")

  return $ Start $ ClockSettings wt sbt lbt cs lbf

statusParser :: Parser Command
statusParser = Message . Status <$> strOption (long "format" <> short 'f' <> value "{time} ({state}), #{cycle}")

commandParser :: Parser Command
commandParser = subparser
  ( command "start"  (info (startParser  <**> helper) (progDesc "Start the timer"))
 <> command "status" (info (statusParser <**> helper) (progDesc "Show the status"))
 <> command "stop"   (info (pure (Message Terminate) <**> helper) (progDesc "Terminates the deamon"))
 <> command "toggle" (info (pure (Message Toggle   ) <**> helper) (progDesc "Toggle the clock"))
  )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "A simple timer application"
 <> header "timer - a simple timer application"
  )

parseCommand :: IO Command
parseCommand = execParser opts
