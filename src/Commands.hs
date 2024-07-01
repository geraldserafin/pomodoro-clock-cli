{-# LANGUAGE DeriveGeneric #-}

module Commands where

import Options.Applicative
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Command
  = StartDeamon   
  | MessageDeamon Message 

data Message 
  = Start { workTime :: Int, breakTime :: Int, cycles :: Int }
  | Status
  deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

startParser :: Parser Command
startParser = MessageDeamon <$> (Start
  <$> option auto
      ( long    "work-time"
     <> short   'w'
     <> value   25
     <> metavar "INT"
     <> help    "Time in minutes for work" 
      )

  <*> option auto
      ( long    "break-time"
     <> short   'b'
     <> value   5
     <> metavar "INT"
     <> help    "Time in minutes for break" 
      )  

  <*> option auto
      ( long    "cycles"
     <> short   'c'
     <> value   1
     <> metavar "INT"
     <> help    "How many cycles to do" 
      ))

statusParser :: Parser Command
statusParser = pure $ MessageDeamon Status

deamonCommandsParser :: Parser Command
deamonCommandsParser = subparser 
  ( command "start" (info (pure StartDeamon <**> helper) (progDesc "Starts the deamon"))
  )

commandParser :: Parser Command
commandParser = subparser
  ( command "start"  (info (startParser  <**> helper) (progDesc "Start the timer"))
 <> command "status" (info (statusParser <**> helper) (progDesc "Show the status"))
 <> command "deamon" (info (deamonCommandsParser <**> helper) (progDesc "Manage the deamon"))
  )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "A simple timer application"
 <> header "timer - a simple timer application" 
  )

parseCommand :: IO Command
parseCommand = execParser opts
