{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Commands
  ( Configuration (..)
  , ClockMessage (..)
  , ClockSettings (..)
  , Command (..)
  , HooksSettings (..)
  , StatusSettings (..)
  , Interaction (Interaction)
  , parseCommand
  )
where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , object
  , withObject
  , (.!=)
  , (.:?)
  )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Yaml (decodeFileEither)
import Data.Yaml.TH (FromJSON (parseJSON))
import GHC.Generics (Generic)
import Options.Applicative
import System.Directory.Extra (doesFileExist, getHomeDirectory)

data ClockSettings = ClockSettings
  { title :: Maybe String
  , workTime :: Float
  , shortBreakTime :: Float
  , longBreakTime :: Float
  , cycles :: Int
  , longBreakFrequency :: Int
  }
  deriving (Show, Generic)

data HooksSettings = HooksSettings
  { onWorkStart :: Maybe String
  , onBreakStart :: Maybe String
  , onPomodoroEnd :: Maybe String
  }
  deriving (Show, Generic)

data StatusSettings = StatusSettings
  { format :: String
  , workText :: String
  , shortBreakText :: String
  , longBreakText :: String
  }
  deriving (Show, Generic)

data Configuration = Configuration
  { clockSettings :: ClockSettings
  , statusSettings :: StatusSettings
  , hooksSettings :: HooksSettings
  , socketPath :: String
  }
  deriving (Generic, Show)

data ClockMessage = Terminate | Status StatusSettings | Toggle
  deriving (Show, Generic)

data Command
  = Start Configuration
  | Message ClockMessage

data Interaction = Interaction Command String

defaultConfig :: Configuration
defaultConfig =
  Configuration
    { clockSettings =
        ClockSettings
          { title = Nothing
          , workTime = 25
          , shortBreakTime = 5
          , longBreakTime = 15
          , cycles = 1
          , longBreakFrequency = 4
          }
    , statusSettings =
        StatusSettings
          { format = "{title} {time} ({state}), {cycle}/{goal}"
          , workText = "Work"
          , shortBreakText = "Short Break"
          , longBreakText = "Long Break"
          }
    , hooksSettings =
        HooksSettings
          { onWorkStart = Nothing
          , onBreakStart = Nothing
          , onPomodoroEnd = Nothing
          }
    , socketPath = "/tmp/pomodoro.sock"
    }

$(deriveJSON defaultOptions ''StatusSettings)
$(deriveJSON defaultOptions ''ClockMessage)
$(deriveJSON defaultOptions ''ClockSettings)
$(deriveJSON defaultOptions ''HooksSettings)

instance ToJSON Configuration

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \v -> do
    hooksSettings <- v .:? "hooksSettings" .!= defaultConfig.hooksSettings
    socketPath <- v .:? "socketPath" .!= defaultConfig.socketPath
    statusSettings <-
      v .:? "statusSettings" .!= object []
        >>= withObject
          "StatusSettings"
          ( \o ->
              do
                format <- o .:? "format" .!= defaultStatusSettings.format
                workText <- o .:? "workText" .!= defaultStatusSettings.workText
                shortBreakText <-
                  o .:? "shortBreakText" .!= defaultStatusSettings.shortBreakText
                longBreakText <- o .:? "longBreakText" .!= defaultStatusSettings.longBreakText
                return StatusSettings{..}
          )
    clockSettings <-
      v .:? "clockSettings" .!= object []
        >>= withObject
          "ClockSettings"
          ( \o -> do
              title <- o .:? "title" .!= defaultClockSettings.title
              workTime <- o .:? "workTime" .!= defaultClockSettings.workTime
              shortBreakTime <- o .:? "breakTime" .!= defaultClockSettings.shortBreakTime
              longBreakTime <- o .:? "longBreakTime" .!= defaultClockSettings.longBreakTime
              cycles <- o .:? "cycles" .!= defaultClockSettings.cycles
              longBreakFrequency <-
                o .:? "longBreakFrequency" .!= defaultClockSettings.longBreakFrequency
              return ClockSettings{..}
          )
    return Configuration{..}
    where
      defaultClockSettings = defaultConfig.clockSettings
      defaultStatusSettings = defaultConfig.statusSettings

startParser :: Configuration -> Parser Command
startParser config = do
  let defaults = config.clockSettings

  title <-
    optional $ argument str (metavar "STRING" <> help "The task to be done")

  workTime <-
    option
      auto
      ( long "work-time"
          <> short 'w'
          <> value defaults.workTime
          <> metavar "INT"
          <> help "Time in minutes for work"
      )

  shortBreakTime <-
    option
      auto
      ( long "short-break-time"
          <> short 's'
          <> value defaults.shortBreakTime
          <> metavar "INT"
          <> help "Time in minutes for short break"
      )

  longBreakTime <-
    option
      auto
      ( long "long-break-time"
          <> short 'l'
          <> value defaults.longBreakTime
          <> metavar "INT"
          <> help "Time in minutes for long break"
      )

  cycles <-
    option
      auto
      ( long "cycles"
          <> short 'c'
          <> value defaults.cycles
          <> metavar "INT"
          <> help "How many cycles to do"
      )

  longBreakFrequency <-
    option
      auto
      ( long "long-break-freq"
          <> short 'f'
          <> value defaults.longBreakFrequency
          <> metavar "INT"
          <> help "Every nth cycle to have a longer break"
      )

  return $
    Start $
      config{clockSettings = ClockSettings{..}{title = title <|> defaults.title}}

statusParser :: StatusSettings -> Parser Command
statusParser settings = do
  format <- strOption (long "format" <> short 'f' <> value settings.format)
  workText <- strOption (long "workText" <> value settings.workText)
  shortBreakText <-
    strOption (long "shortBreakText" <> value settings.shortBreakText)
  longBreakText <-
    strOption (long "longBreakText" <> value settings.longBreakText)

  return $ Message . Status $ StatusSettings{..}

commandParser :: Configuration -> Parser Interaction
commandParser config = do
  cmd <-
    subparser
      ( command
          "start"
          (info (startParser config <**> helper) (progDesc "Start the timer"))
          <> command
            "status"
            ( info
                (statusParser config.statusSettings <**> helper)
                (progDesc "Show the status")
            )
          <> command
            "stop"
            (info (pure (Message Terminate) <**> helper) (progDesc "Terminates the deamon"))
          <> command
            "toggle"
            (info (pure (Message Toggle) <**> helper) (progDesc "Toggle the clock"))
      )

  socketPath <- strOption (long "socket" <> value config.socketPath)

  return $ Interaction cmd socketPath

opts :: Parser Interaction -> ParserInfo Interaction
opts parser =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "A simple timer application"
        <> header "timer - a simple timer application"
    )

getConfigPath :: IO FilePath
getConfigPath = do
  homeDir <- getHomeDirectory
  return $ homeDir <> "/.config/pomodoro/config.yaml"

readConfig :: IO Configuration
readConfig = do
  configPath <- getConfigPath
  exists <- doesFileExist configPath
  if not exists
    then return defaultConfig
    else do
      result <- decodeFileEither configPath
      case result of
        Left _ -> return defaultConfig
        Right config -> return config

parseCommand :: IO Interaction
parseCommand = do
  config <- readConfig
  execParser . opts $ commandParser config
