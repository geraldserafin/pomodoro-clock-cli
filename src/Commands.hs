{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}

module Commands where

import Options.Applicative
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Command
  = StartDeamon   
  | MessageDeamon Message 

data ClockSettings = ClockSettings
  { workTime :: Int
  , breakTime :: Int
  , cycles :: Int
  } deriving (Show, Generic)

instance ToJSON ClockSettings
instance FromJSON ClockSettings

data Message 
  = Start ClockSettings 
  | Status
  deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

startParser :: Parser Command
startParser = do 
  wt <- option auto (long "work-time"  <> short 'w' <> value 25 <> metavar "INT" <> help "Time in minutes for work" )
  bt <- option auto (long "break-time" <> short 'b' <> value 5  <> metavar "INT" <> help "Time in minutes for break")  
  cs <- option auto (long "cycles"     <> short 'c' <> value 1  <> metavar "INT" <> help "How many cycles to do"    )

  return $ MessageDeamon $ Start $ ClockSettings wt bt cs

statusParser :: Parser Command
statusParser = pure $ MessageDeamon Status

commandParser :: Parser Command
commandParser = subparser
  ( command "start"  (info (startParser  <**> helper) (progDesc "Start the timer"))
 <> command "status" (info (statusParser <**> helper) (progDesc "Show the status"))
 <> command "deamon" (info (deamonCommandsParser <**> helper) (progDesc "Manage the deamon"))
  )

deamonCommandsParser :: Parser Command
deamonCommandsParser = subparser 
  ( command "start" (info (pure StartDeamon <**> helper) (progDesc "Starts the deamon"))
  )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "A simple timer application"
 <> header "timer - a simple timer application" 
  )

parseCommand :: IO Command
parseCommand = execParser opts
