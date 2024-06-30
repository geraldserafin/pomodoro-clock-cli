module Commands (parseCommand, Command (Start, Status)) where

import Options.Applicative

data Command
  = Start { workTime :: Int, breakTime :: Int, cycles :: Int }
  | Status


startParser :: Parser Command
startParser = Start
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
      )


statusParser :: Parser Command
statusParser = pure Status


commandParser :: Parser Command
commandParser = subparser
  ( command "start"  (info startParser  (progDesc "Start the timer"))
 <> command "status" (info statusParser (progDesc "Show the status"))
  )


opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "A simple timer application"
 <> header "timer - a simple timer application" 
 )


parseCommand :: IO Command
parseCommand = execParser opts
